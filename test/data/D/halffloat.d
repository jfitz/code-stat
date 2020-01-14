/*
 * References:
 *      http://en.wikipedia.org/wiki/Half-precision_floating-point_format
 */

module halffloat;

struct HF {

    /* Provide implicit conversion of HF to float
     */

    @property float toFloat() { return shortToFloat(s); }
    alias toFloat this;

    /* Done as a template in order to prevent implicit conversion
     * of argument to float.
     */

    this(T : float)(T f)
    {
        static assert(is(T == float));
        s = floatToShort(f);
    }

    /* These are done as properties to avoid
     * circular reference problems.
     */

    static @property HF min_normal() { HF hf = void; hf.s = 0x0400; return hf; /* fp16!0x1p-14; */ }
    static @property HF max()        { HF hf = void; hf.s = 0x7BFF; return hf; /* fp16!0x1.FFCp+15; */ }
    static @property HF nan()        { HF hf = void; hf.s = EXPMASK | 1; return hf; /* fp16!(float.nan); */ }
    static @property HF infinity()   { HF hf = void; hf.s = EXPMASK; return hf; /* fp16!(float.infinity); */ }
    static @property HF epsilon()    { HF hf = void; hf.s = 0x3C01; return hf; /* fp16!0x1p-10; */ }

    enum dig =        3;
    enum mant_dig =   11;
    enum max_10_exp = 5;
    enum max_exp =    16;
    enum min_10_exp = -5;
    enum min_exp =    -14;

  private:
    ushort s = EXPMASK | 1;     // .init is HF.nan
}

/********************
 * User defined literal for Half Float.
 */

template fp16(float v)
{
    enum fp16 = HF(v);
}

private:

// Half float values
enum SIGNMASK  = 0x8000;
enum EXPMASK   = 0x7C00;
enum MANTMASK  = 0x03FF;
enum HIDDENBIT = 0x0400;

// float values
enum FSIGNMASK  = 0x80000000;
enum FEXPMASK   = 0x7F800000;
enum FMANTMASK  = 0x007FFFFF;
enum FHIDDENBIT = 0x00800000;

// Rounding mode
enum ROUND { TONEAREST, UPWARD, DOWNWARD, TOZERO };
enum ROUNDMODE = ROUND.TONEAREST;

union U { uint u; float f; }

ushort floatToShort(float f)
{
    /* If the target CPU has a conversion instruction, this code could be
     * replaced with inline asm or a compiler intrinsic, but leave this
     * as the CTFE path so CTFE can work on it.
     */

    /* The code currently does not set INEXACT, UNDERFLOW, or OVERFLOW,
     * but is marked where those would go.
     */

    U uf = void;
    uf.f = f;
    uint s = uf.u;

    ushort u = (s & FSIGNMASK) ? SIGNMASK : 0;
    int exp = s & FEXPMASK;
    if (exp == FEXPMASK)  // if nan or infinity
    {
        if ((s & FMANTMASK) == 0)       // if infinity
        {
            u |= EXPMASK;
        }
        else                            // else nan
        {
            u |= EXPMASK | 1;
        }
        return u;
    }

    uint significand = s & FMANTMASK;

    if (exp == 0)                       // if subnormal or zero
    {
        if (significand == 0)           // if zero
            return u;

        /* A subnormal float is going to give us a zero result anyway,
         * so just set UNDERFLOW and INEXACT and return +-0.
         */
        return u;
    }
    else                                // else normal
    {
        // normalize exponent and remove bias
        exp = (exp >> 23) - 127;
        significand |= FHIDDENBIT;
    }

    exp += 15;                          // bias the exponent

    bool guard = false;                 // guard bit
    bool sticky = false;                // sticky bit

    uint shift = 13;                    // lop off rightmost 13 bits
    if (exp <= 0)                       // if subnormal
    {   shift += -exp + 1;              // more bits to lop off
        exp = 0;
    }
    if (shift > 23)
    {
        // Set UNDERFLOW, INEXACT, return +-0
        return u;
    }

    // Lop off rightmost 13 bits, but save guard and sticky bits
    guard = (significand & (1 << (shift - 1))) != 0;
    sticky = (significand & ((1 << (shift - 1)) - 1)) != 0;
    significand >>= shift;

    if (guard || sticky)
    {
        // Lost some bits, so set INEXACT and round the result
        switch (ROUNDMODE)
        {
            case ROUND.TONEAREST:
                if (guard && (sticky || (significand & 1)))
                    ++significand;
                break;

            case ROUND.UPWARD:
                if (!(s & FSIGNMASK))
                    ++significand;
                break;

            case ROUND.DOWNWARD:
                if (s & FSIGNMASK)
                    ++significand;
                break;

            case ROUND.TOZERO:
                break;

            default:
                assert(0);
        }
        if (exp == 0)                           // if subnormal
        {
            if (significand & HIDDENBIT)        // and not a subnormal no more
                ++exp;
        }
        else if (significand & (HIDDENBIT << 1))
        {
            significand >>= 1;
            ++exp;
        }
    }

    if (exp > 30)
    {   // Set OVERFLOW and INEXACT, return +-infinity
        return u | EXPMASK;
    }

    /* Add exponent and significand into result.
     */

    u |= exp << 10;                             // exponent
    u |= (significand & ~HIDDENBIT);            // significand

    return u;
}

float shortToFloat(ushort s)
{
    /* If the target CPU has a conversion instruction, this code could be
     * replaced with inline asm or a compiler intrinsic, but leave this
     * as the CTFE path so CTFE can work on it.
     */
    /* This one is fairly easy because there are no possible errors
     * and no necessary rounding.
     */

    int exp = s & EXPMASK;
    if (exp == EXPMASK)  // if nan or infinity
    {
        float f;
        if ((s & MANTMASK) == 0)        // if infinity
        {
            f = float.infinity;
        }
        else                            // else nan
        {
            f = float.nan;
        }
        return (s & SIGNMASK) ? -f : f;
    }

    uint significand = s & MANTMASK;

    if (exp == 0)                       // if subnormal or zero
    {
        if (significand == 0)           // if zero
            return (s & SIGNMASK) ? -0.0f : 0.0f;

        // Normalize by shifting until the hidden bit is 1
        while (!(significand & HIDDENBIT))
        {
            significand <<= 1;
            --exp;
        }
        significand &= ~HIDDENBIT;      // hidden bit is, well, hidden
	exp -= 14;
    }
    else                                // else normal
    {
        // normalize exponent and remove bias
        exp = (exp >> 10) - 15;
    }

    /* Assemble sign, exponent, and significand into float.
     * Don't have to deal with overflow, inexact, or subnormal
     * because the range of floats is big enough.
     */

    assert(-126 <= exp && exp <= 127);  // just to be sure

    //printf("exp = %d, significand = x%x\n", exp, significand);

    uint u = (s & SIGNMASK) << 16;      // sign bit
    u |= (exp + 127) << 23;             // bias the exponent and shift into position
    u |= significand << (23 - 10);

    U uf = void;

    uf.u = u;
    return uf.f;
}

import std.stdio;

void main() {
//    HF h = fp16!27.2f;
//    HF j = cast(HF)( fp16!3.5f + fp16!5 );
    HF f = HF(0.0f);

    f.s = 0x3C00;
    writeln("1 ", cast(float)f);

    f.s = 0x3C01;
    writeln("1.0009765625 ", cast(float)f);
    assert(f == HF.epsilon);

    f.s = 0xC000;
    writeln("-2 ", cast(float)f);

    f.s = 0x7BFF;
    writeln("65504 ", cast(float)f);
    assert(f == HF.max);

    f.s = 0x0400;
    writeln("6.10352e-5 ", cast(float)f);
    assert(f == HF.min_normal);

    f.s = 0x03FF;
    writeln("6.09756e-5 ", cast(float)f);

    f.s = 1;
    writeln("5.96046e-8 ", cast(float)f);

    f.s = 0;
    writeln("0 ", cast(float)f);
    assert(f == 0.0f);

    f.s = 0x8000;
    writeln("-0 ", cast(float)f);
    assert(f == -0.0f);

    f.s = 0x7C00;
    writeln("infinity ", cast(float)f);
    assert(f == HF.infinity);

    f.s = 0xFC00;
    writeln("-infinity ", cast(float)f);
    assert(f == -HF.infinity);

    f.s = 0x3555;
    writeln("0.33325 ", cast(float)f);
}
