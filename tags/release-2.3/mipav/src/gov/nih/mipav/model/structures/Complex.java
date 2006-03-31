package gov.nih.mipav.model.structures;

public class Complex {

    float real = 0.0f;
    float imag = 0.0f;

    public Complex() {
        real = imag = 0.0f;
    }

    public Complex(float r, float i) {
        real = r;
        imag = i;
    }

    public Complex(float r) {
        real = r;
        imag = 0.0f;
    }

    public Complex(Complex val) {
        real = val.real;
        imag = val.imag;
    }

    final public void setReal(float val) {
        real = val;
    }
    final public void setImag(float val) {
        imag = val;
    }

    final public float getReal() {
        return real;
    }
    final public float getImag() {
        return imag;
    }

    final public float magnitude() {
        return (float)(Math.sqrt(real*real + imag*imag));
    }

    final public Complex add(Complex val) {
        return new Complex(this.real + val.real, this.imag + val.imag);
    } // end add(...)

    final public Complex subtract(Complex val) {
        return new Complex(this.real - val.real, this.imag - val.imag);
    } // end subtract(...)

    final public Complex multiply(Complex val) {
        Complex result = new Complex();
        result.real = this.real*val.real - this.imag*val.imag;
        result.imag = this.imag*val.real + this.real*val.imag;
        return result;
    } // end multiply(...)

    final public Complex divide(Complex val) {
        Complex result = new Complex();
        float denom = val.real*val.real + val.imag*val.imag;
        result.real = (this.real*val.real + this.imag*val.imag) / denom;
        result.imag = (this.imag*val.real - this.real*val.imag) / denom;
        return result;
    } // end divide(...)

} // end class Complex
