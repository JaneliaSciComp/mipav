package gov.nih.mipav.model.structures;


/**
 * DOCUMENT ME!
 */
public class Complex {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    float imag = 0.0f;

    /** DOCUMENT ME! */
    float real = 0.0f;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new Complex object.
     */
    public Complex() {
        real = imag = 0.0f;
    }

    /**
     * Creates a new Complex object.
     *
     * @param  r  DOCUMENT ME!
     */
    public Complex(float r) {
        real = r;
        imag = 0.0f;
    }

    /**
     * Creates a new Complex object.
     *
     * @param  val  DOCUMENT ME!
     */
    public Complex(Complex val) {
        real = val.real;
        imag = val.imag;
    }

    /**
     * Creates a new Complex object.
     *
     * @param  r  DOCUMENT ME!
     * @param  i  DOCUMENT ME!
     */
    public Complex(float r, float i) {
        real = r;
        imag = i;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param   val  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final Complex add(Complex val) {
        return new Complex(this.real + val.real, this.imag + val.imag);
    } // end add(...)

    /**
     * DOCUMENT ME!
     *
     * @param   val  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final Complex divide(Complex val) {
        Complex result = new Complex();
        float denom = (val.real * val.real) + (val.imag * val.imag);
        result.real = ((this.real * val.real) + (this.imag * val.imag)) / denom;
        result.imag = ((this.imag * val.real) - (this.real * val.imag)) / denom;

        return result;
    } // end divide(...)

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final float getImag() {
        return imag;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final float getReal() {
        return real;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final float magnitude() {
        return (float) (Math.sqrt((real * real) + (imag * imag)));
    }

    /**
     * DOCUMENT ME!
     *
     * @param   val  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final Complex multiply(Complex val) {
        Complex result = new Complex();
        result.real = (this.real * val.real) - (this.imag * val.imag);
        result.imag = (this.imag * val.real) + (this.real * val.imag);

        return result;
    } // end multiply(...)

    /**
     * DOCUMENT ME!
     *
     * @param  val  DOCUMENT ME!
     */
    public final void setImag(float val) {
        imag = val;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  val  DOCUMENT ME!
     */
    public final void setReal(float val) {
        real = val;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   val  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final Complex subtract(Complex val) {
        return new Complex(this.real - val.real, this.imag - val.imag);
    } // end subtract(...)

} // end class Complex
