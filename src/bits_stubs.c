#include <stdint.h>
#include <inttypes.h>
#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
#else
#include <alloca.h>
#endif
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

/* Perform iunsigned comparison of [Int64.t] as [uint64_t]. */
CAMLprim intnat hardcaml_bits_uint64_compare(uint64_t a, uint64_t b) {
  intnat ltu, gtu;
  ltu = a < b;
  gtu = a > b;
  return gtu - ltu;
}

CAMLprim intnat hardcaml_bits_uint64_compare_bc(value va, value vb) {
  return (Val_int(hardcaml_bits_uint64_compare(Int64_val(va), Int64_val(vb))));
}

/* Zero out any bits above width in the given [Bits.t] */
CAMLprim value hardcaml_bits_mask(intnat width, value vdst) {
  uint64_t *dst = (uint64_t *) String_val(vdst);
  int word = (width - 1) >> 6;
  int bits = width & 0x3f;
  if (bits) {
    uint64_t mask = ~ (0xFFFFFFFFFFFFFFFF << bits);
    dst[word] &= mask;
  }
  return Val_unit;
}

CAMLprim value hardcaml_bits_mask_bc(value width, value vdst) {
  return hardcaml_bits_mask(Int_val(width), vdst);
}

/* Add two multiword [Bits.t].  The widths of the arguments and results are the same.
   Currently done with 128 bit arithmetic (giving 64 bits per iteration, plus the carry
   bit).  64 bit may actually be faster. */
CAMLprim value hardcaml_bits_add(intnat width, value vdst, value va, value vb) {
  uint64_t *dst = (uint64_t *) String_val(vdst);
  uint64_t *a = (uint64_t *) String_val(va);
  uint64_t *b = (uint64_t *) String_val(vb);
  __uint128_t carry = 0;
  int index = 0;

  /* short cut for common case */
  if (width <= 64) {
    dst[0] = a[0] + b[0];
    return Val_unit;
  }

  while (width > 0) {
    __uint128_t x = (__uint128_t) a[index] + (__uint128_t) b[index] + carry;
    dst[index] = (uint64_t) x;
    carry = x >> 64;
    index++;
    width -= 64;
  }

  return Val_unit;
}

CAMLprim value hardcaml_bits_add_bc(value width, value vdst, value va, value vb) {
  return hardcaml_bits_add(Int_val(width), vdst, va, vb);
}

/* Multiword subtraction.  Arguments and result are the same width. */
CAMLprim value hardcaml_bits_sub(intnat width, value vdst, value va, value vb) {
  uint64_t *dst = (uint64_t *) String_val(vdst);
  uint64_t *a = (uint64_t *) String_val(va);
  uint64_t *b = (uint64_t *) String_val(vb);
  __uint128_t borrow = 0;
  int index = 0;

  /* short cut for common case */
  if (width <= 64) {
    dst[0] = a[0] - b[0];
    return Val_unit;
  }

  while (width > 0) {
    __uint128_t x = (__uint128_t) a[index] - (__uint128_t) b[index] - borrow;
    dst[index] = (uint64_t) x;
    borrow = (x >> 64) & 1;
    index++;
    width -= 64;
  }

  return Val_unit;
}

CAMLprim value hardcaml_bits_sub_bc(value width, value vdst, value va, value vb) {
  return hardcaml_bits_sub(Int_val(width), vdst, va, vb);
}

/* Test bit at (width-1) and return bits that should be or'd with [a] to sign
 * extend it. */
static uint64_t sign_mask(int width, uint64_t *a) {
  int word;
  int bit;
  int is_negative;

  width = width - 1;
  word = width >> 6;
  bit = width & 0x3f;
  is_negative = (a[word] >> bit) & 1;

  return is_negative ? ((uint64_t) 0xFFFFFFFFFFFFFFFF) << bit : 0;
}

/* Multiword unsigned multiplication done 32 bits per iteration. 
 *
 * This is obfuscated due to a direct translation from Fortran.
 *
 * [w] is the destination, and [u], [v] are inputs with corresponding lengths
 * [m], [n]. 
 * */
static void mulu(uint32_t *w, uint32_t *u, uint32_t *v, int m, int n)
{ uint64_t k, t; int i, j;

  for (j = 0; j < n; j++) {
    k = 0;
    for (i = 0; i < m; i++) {
      t = ((uint64_t) u[i] * (uint64_t) v[j]) + (uint64_t) w[i + j] + k;
      w[i + j] = t;
      k = t >> 32;
    }
    w[j + m] = k;
  }
  return;
}

/* Multiword signed multiplication - corrects the unsigned result. Inputs must
 * be sign extended to 32 bits. 
 *
 * Arguments are the same as [mulu].
 * */
void muls(uint32_t *w, uint32_t *u, uint32_t *v, int m, int n) {
  uint64_t t, b;
  int i, j;

  mulu(w, u, v, m, n);

  if ((int32_t)u[m - 1] < 0) {
    b = 0;
    for (j = 0; j < n; j++) {
      t = (uint64_t) w[j + m] - (uint64_t) v[j] - b;
      w[j + m] = t;
      b = t >> 63;
    }
  }
  if ((int32_t)v[n - 1] < 0) {
    b = 0;
    for (i = 0; i < m; i++) {
      t = (uint64_t) w[i + n] - (uint64_t) u[i] - b;
      w[i + n] = t;
      b = t >> 63;
    }
  }
  return;
}

/* Unsigned multiplication.  Result is computed in a temporary array on the
 * stack as the output width may not be exactly the width required for the
 * computation. */
static void hardcaml_bits_umul_i(uint64_t *dst, uint64_t *_a, uint64_t *_b,
                                 intnat width_a, intnat width_b) {
  intnat words_a = (width_a + 63) >> 6;
  intnat words_b = (width_b + 63) >> 6;
  intnat words_r = words_a + words_b;
  intnat words_dst = (width_a + width_b + 63) >> 6; /* not always == words_r */
  /* Allocate temporary result arrays on the stack, clear and initialize them. */
  intnat bytes_a = sizeof(uint64_t) * words_a;
  intnat bytes_b = sizeof(uint64_t) * words_b;
  intnat bytes_r = sizeof(uint64_t) * words_r;
  uint64_t *r = alloca(bytes_r);

  memset(r, 0, bytes_r);

  mulu((uint32_t *) r,
       (uint32_t *) _a,
       (uint32_t *) _b,
       bytes_a >> 2, bytes_b >> 2);

  memcpy(dst, r, sizeof(uint64_t) * words_dst);
}

CAMLprim value hardcaml_bits_umul(value dst, value a, value b,
                                  intnat width_a, intnat width_b) {
  hardcaml_bits_umul_i((uint64_t *) String_val(dst),
                       (uint64_t *) String_val(a),
                       (uint64_t *) String_val(b),
                       width_a,
                       width_b);
  return Val_unit;
}

CAMLprim value hardcaml_bits_umul_bc(value dst, value a, value b,
                                     value width_a, value width_b) {
  return hardcaml_bits_umul(dst, a, b, Int_val(width_a), Int_val(width_b));
}


/* Signed multiplication. Arguments are copied to temporary arrays so they can be modified
   for signed extension as necesseary. */
static void hardcaml_bits_smul_i(uint64_t * dst, uint64_t * _a, uint64_t * _b,
                                 intnat width_a, intnat width_b) {
  intnat words_a = (width_a + 63) >> 6;
  intnat words_b = (width_b + 63) >> 6;
  intnat words_r = words_a + words_b;
  intnat words_dst = (width_a + width_b + 63) >> 6; /* not always == words_r */
  /* Allocate temporary result arrays on the stack, clear and initialize them. */
  intnat bytes_a = sizeof(uint64_t) * words_a;
  intnat bytes_b = sizeof(uint64_t) * words_b;
  intnat bytes_r = sizeof(uint64_t) * words_r;
  uint64_t *a = alloca(bytes_a);
  uint64_t *b = alloca(bytes_b);
  uint64_t *r = alloca(bytes_r);

  uint64_t sign_mask_a;
  uint64_t sign_mask_b;

  memset(r, 0, bytes_r);
  memcpy(a, _a, bytes_a);
  sign_mask_a = sign_mask(width_a, a);
  a[words_a-1] = a[words_a-1] | sign_mask_a;

  memcpy(b, _b, bytes_b);
  sign_mask_b = sign_mask(width_b, b);
  b[words_b-1] = b[words_b-1] | sign_mask_b;

  muls((uint32_t *) r,
       (uint32_t *) a,
       (uint32_t *) b,
       bytes_a >> 2, bytes_b >> 2);

  memcpy(dst, r, sizeof(uint64_t) * words_dst);
}

CAMLprim value hardcaml_bits_smul(value dst, value a, value b,
                                  intnat width_a, intnat width_b) {
  hardcaml_bits_smul_i((uint64_t *) String_val(dst),
                       (uint64_t *) String_val(a),
                       (uint64_t *) String_val(b),
                       width_a, 
                       width_b);
  return Val_unit;
}

CAMLprim value hardcaml_bits_smul_bc(value dst, value a, value b,
                                     value width_a, value width_b) {
  return hardcaml_bits_smul(dst, a, b, Int_val(width_a), Int_val(width_b));
}
