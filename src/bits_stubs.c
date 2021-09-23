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

static inline uint64_t mask_last_word(uint64_t word, int width) {
  int bits = width & 0x3f;
  if (bits) {
    uint64_t mask = ~ (0xFFFFFFFFFFFFFFFF << bits);
    return word & mask;
  }
  return word;
}

/* Zero out any bits above width in the given [Bits.t] */
CAMLprim value hardcaml_bits_mask(intnat width, value vdst) {
  uint64_t *dst = (uint64_t *) String_val(vdst);
  int word = (width - 1) >> 6;
  dst[word] = mask_last_word(dst[word], width);
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
    dst[0] = mask_last_word(dst[0], width);
    return Val_unit;
  }

  while (width > 0) {
    __uint128_t x = (__uint128_t) a[index] + (__uint128_t) b[index] + carry;
    dst[index] = (uint64_t) x;
    carry = x >> 64;
    index++;
    width -= 64;
  }
  dst[index-1] = mask_last_word(dst[index-1], width);

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
    dst[0] = mask_last_word(dst[0], width);
    return Val_unit;
  }

  while (width > 0) {
    __uint128_t x = (__uint128_t) a[index] - (__uint128_t) b[index] - borrow;
    dst[index] = (uint64_t) x;
    borrow = (x >> 64) & 1;
    index++;
    width -= 64;
  }

  dst[index-1] = mask_last_word(dst[index-1], width);
  return Val_unit;
}

CAMLprim value hardcaml_bits_sub_bc(value width, value vdst, value va, value vb) {
  return hardcaml_bits_sub(Int_val(width), vdst, va, vb);
}

CAMLprim value hardcaml_bits_and(intnat width, value vdst, value va, value vb) {
  uint64_t *dst = (uint64_t *) String_val(vdst);
  uint64_t *a = (uint64_t *) String_val(va);
  uint64_t *b = (uint64_t *) String_val(vb);
  int index = 0;

  while (width > 0) {
    dst[index] = (uint64_t) a[index] & b[index];
    index++;
    width -= 64;
  }

  return Val_unit;
}

CAMLprim value hardcaml_bits_and_bc(value width, value vdst, value va, value vb) {
  return hardcaml_bits_and(Int_val(width), vdst, va, vb);
}

CAMLprim value hardcaml_bits_or(intnat width, value vdst, value va, value vb) {
  uint64_t *dst = (uint64_t *) String_val(vdst);
  uint64_t *a = (uint64_t *) String_val(va);
  uint64_t *b = (uint64_t *) String_val(vb);
  int index = 0;

  while (width > 0) {
    dst[index] = (uint64_t) a[index] | b[index];
    index++;
    width -= 64;
  }

  return Val_unit;
}

CAMLprim value hardcaml_bits_or_bc(value width, value vdst, value va, value vb) {
  return hardcaml_bits_or(Int_val(width), vdst, va, vb);
}

CAMLprim value hardcaml_bits_xor(intnat width, value vdst, value va, value vb) {
  uint64_t *dst = (uint64_t *) String_val(vdst);
  uint64_t *a = (uint64_t *) String_val(va);
  uint64_t *b = (uint64_t *) String_val(vb);
  int index = 0;

  while (width > 0) {
    dst[index] = (uint64_t) a[index] ^ b[index];
    index++;
    width -= 64;
  }

  return Val_unit;
}

CAMLprim value hardcaml_bits_xor_bc(value width, value vdst, value va, value vb) {
  return hardcaml_bits_xor(Int_val(width), vdst, va, vb);
}

CAMLprim value hardcaml_bits_not(intnat width_in, value vdst, value va) {
  uint64_t *dst = (uint64_t *) String_val(vdst);
  uint64_t *a = (uint64_t *) String_val(va);
  int index = 0;
  intnat width = width_in;

  while (width > 0) {
    dst[index] = ~ a[index];
    index++;
    width -= 64;
  }

  dst[index-1] = mask_last_word(dst[index-1], width_in);

  return Val_unit;
}

CAMLprim value hardcaml_bits_not_bc(value width, value vdst, value va) {
  return hardcaml_bits_not(Int_val(width), vdst, va);
}

CAMLprim value hardcaml_bits_eq(intnat width, value vdst, value va, value vb) {
  uint64_t *dst = (uint64_t *) String_val(vdst);
  uint64_t *a = (uint64_t *) String_val(va);
  uint64_t *b = (uint64_t *) String_val(vb);
  int index = 0;

  dst[0] = 1;
  while (width > 0) {
    if (a[index] != b[index]) {
      dst[0] = 0;
      return Val_unit;
    }
    index++;
    width -= 64;
  }

  return Val_unit;
}

CAMLprim value hardcaml_bits_eq_bc(value width, value vdst, value va, value vb) {
  return hardcaml_bits_eq(Int_val(width), vdst, va, vb);
}

CAMLprim value hardcaml_bits_lt(intnat width, value vdst, value va, value vb) {
  uint64_t *dst = (uint64_t *) String_val(vdst);
  uint64_t *a = (uint64_t *) String_val(va);
  uint64_t *b = (uint64_t *) String_val(vb);
  int word = (width - 1) >> 6;

  dst[0] = 0;
  while (word >= 0) {
    /* If equal, continue to next word */
    if (a[word] != b[word]) {
      /* If less, then the result is true; If false, then result is false. */
      dst[0] = a[word] < b[word];
      return Val_unit;
    }
    word--;
  }

  return Val_unit;
}

CAMLprim value hardcaml_bits_lt_bc(value width, value vdst, value va, value vb) {
  return hardcaml_bits_lt(Int_val(width), vdst, va, vb);
}

CAMLprim value hardcaml_bits_select(value vdst, value vsrc, intnat high, intnat low) {
  uint64_t *dst = (uint64_t *) String_val(vdst);
  uint64_t *src = (uint64_t *) String_val(vsrc);
  intnat width = high - low + 1;

  /* Short cut - target bits are contained within 1ast 64 bit word */
  if ((width+low) <= 64) {
    dst[0] = (src[0] >> low);
    dst[0] = mask_last_word(dst[0], width);
    return Val_unit;
  }

  intnat words = (width + 63) >> 6;
  intnat bits = low & 63;
  intnat low_word = low >> 6;
  intnat high_word = high >> 6;
  intnat i;

  if (bits == 0) {
    /* If first selected bit position is 64-bit aligned, use short-circuit
       that skip all the bit shifting
     */
    for (i=0; i<words; i++) {
      dst[i] = src[low_word + i];
    }

  } else {
    /* The following routine loops through [words] words in [src], can
       concatenate the upper [bits] of the src[low_word+i] with the bottom
       [64-bits] of [low_word+i+1]. src[low_word_i] is conveniently buffered
       in the [a] variable.
     */

    uint64_t a = src[low_word];

    for (i=0; i<words; i++) {
      uint64_t b = low_word+i >= high_word ? 0 : src[low_word+i+1];
      dst[i] = (b << (64 - bits)) | (a >> bits);
      a = b;
    }
  }

  dst[i-1] = mask_last_word(dst[i-1], width);
  return Val_unit;
}

CAMLprim value hardcaml_bits_select_bc(value vdst, value vsrc, value high, value low) {
  return hardcaml_bits_select(vdst, vsrc, Int_val(high), Int_val(low));
}

CAMLprim intnat hardcaml_bits_cat2(value va, intnat a_width,
                                   value vb, intnat b_width) {
  uint64_t *a = (uint64_t *) String_val(va);
  uint64_t *b = (uint64_t *) String_val(vb);
  intnat a_words = (a_width + 63) >> 6;
  intnat b_words = (b_width + 63) >> 6;
  intnat a_bits = a_width & 63;
  intnat i;

  /* If the next bit to write to the is 64-bit-aligned, skip the unnecessary
     bit shifts.
  */
  if (a_bits == 0) {
    for (i=0; i<b_words; i++) {
      a[a_words+i] = b[i];
    }
    return Val_unit;
  }

  /* The general case where next bit to write is not 64-bit aligned follows. */

  /* The below memory access is always safe, as (a_bits != 0 --> a_words > 0).
     This buffers the first word in [a], to be bitwise OR-ed with data from
     [b].
   */
  uint64_t x = a[a_words - 1];

  /* The following loop takes the bottom [64 - a_bits] and OR it with [a_bits]
     from either an earlier word or the existing data in [a]. [x] is then
     updated to contain the uppermost [a_bits] from this word in [b].
   */
  for (i=0; i<b_words; i++) {
    uint64_t y = b[i];
    a[a_words - 1 + i] = x | (y << a_bits);
    x = y >> (64 - a_bits);
  }

  /* [x] contains residual data, that is either the a[words - 1] (when b_words
     = 0), or the upper [a_bits] of the last 64-bit word of [b]'s data.

     The following conditional checks if the residual word is within the bound
     of [b].
   */
  intnat num_bits_in_last_word = b_width & 63;
  num_bits_in_last_word = (num_bits_in_last_word == 0) ? 64 : num_bits_in_last_word;
  if (num_bits_in_last_word > 64 - a_bits) {
    a[a_words + b_words - 1] = x;
  }

  return Val_unit;
}

CAMLprim value hardcaml_bits_cat2_bc(value a, value a_width,
                                     value b, value b_width) {
  return hardcaml_bits_cat2(a, Int_val(a_width),
                            b, Int_val(b_width));
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
