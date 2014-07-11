package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import java.awt.Color;
import java.awt.event.*;
import java.io.IOException;


/**
 * ViewJComponent DTI Image
 * 
 * @author pandyan
 * 
 * References: Developed in concert with Sinisa Pajevic from the NIH/CIT/DCB/MSCL group, Lin-Ching Chang D.Sc., Carlo
 * Pierpaoli MD Ph.D., and Lindsay Walker MS from the the NIH/NICHD/LIMB/STBB group and Olga Vovk from the
 * NIH/CIT/DCB/ISL/BIRSS group:
 * 
 * 
 * Mathematical and Statistical Computing Laboratory (MSCL) Biomedical Imaging Research Services Section (BIRSS) Imaging
 * Sciences Laboratory (ISL) Division of Cumputational Bioscience (DCB) Center for Informational Technology (CIT)
 * Section on Tissue Biophysics and Biomimetics (STBB) Laboratory of Integrative and Medical Biophysics (LIMB) National
 * Institute of Child Health & Humann Development National Institutes of Health
 * 
 * 
 * Publication Reference:
 * 
 * S. Pajevic and C. Pierpaoli, "Color Schemes to Represent the Orientation of Anisotropic Tissues from Diffusion Tensor
 * Data: Application to White Matter Fiber Tract Mapping in the Human Brain," Magnetic Resonance in Medicine, vol. 42,
 * no. 3, pp. 526-540, 1999
 * 
 */
public class ViewJComponentDTIImage extends ViewJComponentEditImage {

    /** type of color wheel ABSVAL, NOSYMM, ROTATIONALSYMM, MIRRORSYMM * */
    private String type;

    /** model image * */
    private ModelImage imageA;

    /** sat vs theta * */
    private float pS = 0.5f;

    /** color range * */
    private float pC = .700f;

    /** blue saturation * */
    private float pB = .350f;

    /** green adj * */
    private float pG = .800f;

    /** gamma correction * */
    private float gamma = 1.8f;

    /** two pi * */
    private float twoPi = 2 * (float) Math.PI;

    /** pi div two * */
    private float piDivTwo = (float) Math.PI / 2;

    /** arry of r,g,b after blue shift * */
    private float blueShiftColors[] = new float[3];

    /** array of r,g,b values after red shifting * */
    private float[] redShiftColors = new float[3];

    /** arry of r,g,b after green adj * */
    private float greenAdjColors[] = new float[3];

    /** anisotropy max * */
    private float anisotropyMax = .7f;

    /** anisotropy min * */
    private float anisotropyMin = 0f;

    /** anisotropy file data buffer * */
    private float[] anisotropyBuffer;

    /** anisotropy file data buffer * */
    private float[] clippedBuffer;

    /** Stevens Beta * */
    private float stevensBeta = .4f;

    /** adjust exp * */
    private float adjustExp = .5f;

    /** boolean for truncate/multiply * */
    private boolean isMultiply = true;

    /** arry of r,g,b after scaling/truncating * */
    private float truncMultColors[] = new float[3];

    /**
     * constructor
     * 
     * @param _frame
     * @param _imageA
     * @param _LUTa
     * @param imgBufferA
     * @param pixelBuffer
     * @param zoom
     * @param extents
     * @param logMagDisplay
     * @param _orientation
     */
    public ViewJComponentDTIImage(ViewJFrameBase _frame, ModelImage _imageA, ModelLUT _LUTa, float[] imgBufferA,
            int[] pixelBuffer, float zoom, int[] extents, boolean logMagDisplay, int _orientation,
            float[] anisotropyBuffer) {

        super(_frame, _imageA, _LUTa, imgBufferA, null, null, null, pixelBuffer, zoom, extents, logMagDisplay,
                _orientation);
        this.imageA = _imageA;
        this.anisotropyBuffer = anisotropyBuffer;
        clippedBuffer = new float[anisotropyBuffer.length];

    }

    /**
     * For generating the display of 1 or 2 images.
     * 
     * @param tSlice t (time) slice to show
     * @param zSlice z slice to show
     * @param forceShow forces this method to import image and recalculate java image
     * 
     * @return boolean to indicate if the show was successful
     */
    public boolean show(int tSlice, int zSlice, boolean forceShow, String type, float pS, float pB, float pC, float pG,
            float gamma, float anisotropyMin, float anisotropyMax, float stevensBeta, float adjustExp,
            boolean isMultiply) {
        this.type = type;
        this.pS = pS;
        this.pB = pB;
        this.pC = pC;
        this.pG = pG;
        this.gamma = gamma;
        this.anisotropyMin = anisotropyMin;
        this.anisotropyMax = anisotropyMax;
        this.stevensBeta = stevensBeta;
        this.adjustExp = adjustExp;
        this.isMultiply = isMultiply;
        return show(tSlice, zSlice, null, null, forceShow, interpMode);
    }

    /**
     * Shows the image and the VOI(s).
     * 
     * @param tSlice t (time) slice to show
     * @param zSlice z slice to show
     * @param _LUTa LUTa - to change to new LUT for imageA else null
     * @param _LUTb LUTb - to change to new LUT for imageB else null
     * @param forceShow forces this method to import image and recalculate java image
     * @param interpMode image interpolation method (Nearest or Smooth)
     * 
     * @return boolean to indicate if the show was successful
     */
    public boolean show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow, int interpMode) {
        // System.out.println("in show " + isMultiply + " " + stevensBeta + " " + adjustExp);

        if (interpMode > -1) {
            setInterpolationMode(interpMode);
        }

        m_kPatientSlice.setLUTa(_LUTa);
        m_kPatientSlice.setLUTb(_LUTb);
        m_kPatientSlice.updateSlice(zSlice);

        if (cleanImageBufferB == null) {
            cleanImageBufferB = new int[imageExtents[0] * imageExtents[1]];
        }

        if (m_kPatientSlice.showUsingOrientation(tSlice, cleanImageBufferA, cleanImageBufferB, forceShow, false)) {

            if ( (pixBuffer == null) || (pixBuffer.length != cleanImageBufferA.length)) {
                pixBuffer = new int[cleanImageBufferA.length];
            }

            // now we need to show depending on the color wheel selected
            // since this is ARGB image....it is alpha, red, green, blue...so we mult by 4
            int length = imageA.getSliceSize() * 4;
            int start = zSlice * length;
            float[] buff = new float[length];
            try {
                imageA.exportData(start, length, buff);
            } catch (IOException e) {

            }

            // first lets clip the anisotropyBuffer depending on anisotropy max and min values
            clipAnisotropyBuffer();

            // call appropriate method based on color wheel selected
            if (type.equals("ABSVAL")) {
                absoluteValue(zSlice, buff);
            }
            if (type.equals("NOSYMM")) {
                noSymm(zSlice, buff);
            }
            if (type.equals("ROTATIONALSYMM")) {
                rotationalSymm(zSlice, buff);
            }
            if (type.equals("MIRRORSYMM")) {
                mirrorSymm(zSlice, buff);
            }

            System.arraycopy(cleanImageBufferA, 0, pixBuffer, 0, cleanImageBufferA.length);
            slice = zSlice;
            setSliceString(String.valueOf(slice));
            paintComponent(getGraphics());

            return true;
        } else {
            return false;
        }
    }

    /**
     * absolute value
     * 
     * @param zSlice
     * @param buff
     */
    public void absoluteValue(int zSlice, float[] buff) {
        float r, g, b;
        int red, green, blue;
        int val, index;
        for (int i = 0, j = (zSlice * buff.length) / 4; i <= (buff.length - 4); i = i + 4, j++) {

            r = Math.abs(buff[i + 1]);
            g = Math.abs(buff[i + 2]);
            b = Math.abs(buff[i + 3]);

            // shift blue
            if (r != 0 && g != 0 && b != 0) {
                blueShiftColors = shiftBlue(r, g, b);
                r = blueShiftColors[0];
                g = blueShiftColors[1];
                b = blueShiftColors[2];
            }

            // shift red
            if (r != 0 && g != 0 && b != 0) {
                redShiftColors = shiftRed(r, g, b);
                r = redShiftColors[0];
                g = redShiftColors[1];
                b = redShiftColors[2];
            }

            // adjust green
            if (r != 0 && g != 0 && b != 0) {
                greenAdjColors = adjustGreen(r, g, b);
                r = greenAdjColors[0];
                g = greenAdjColors[1];
                b = greenAdjColors[2];
            }

            // truncate
            truncMultColors = truncateRGB(r, g, b, clippedBuffer[j]);
            r = truncMultColors[0];
            g = truncMultColors[1];
            b = truncMultColors[2];

            // gamma correction
            r = (float) Math.pow(r, (1 / gamma));
            g = (float) Math.pow(g, (1 / gamma));
            b = (float) Math.pow(b, (1 / gamma));

            // now get r,g,b to 0 - 255 range
            red = Math.round(r * 255);
            green = Math.round(g * 255);
            blue = Math.round(b * 255);

            // now do the orring
            val = 0xff000000 | (red << 16) | (green << 8) | (blue);

            // now put replace in cleanImageBufferA
            index = i / 4;
            cleanImageBufferA[index] = val;

        }
    }

    /**
     * no symmetry
     * 
     * @param zSlice
     * @param buff
     */
    public void noSymm(int zSlice, float[] buff) {
        float vx, vy, vz;
        float hue, sat;
        float theta, phi;
        float xylength;
        int val, index;
        float r, g, b;
        int red, green, blue;
        for (int i = 0, j = (zSlice * buff.length) / 4; i <= (buff.length - 4); i = i + 4, j++) {
            vx = buff[i + 1];
            vy = buff[i + 2];
            vz = buff[i + 3];
            if (vx == 0 && vy == 0 && vz == 0) {
                red = 0;
                green = 0;
                blue = 0;
            } else {
                if (vz < 0) {
                    vx = -vx;
                    vy = -vy;
                    vz = -vz;
                }

                xylength = (float) Math.sqrt( (vx * vx) + (vy * vy));

                theta = (float) Math.asin(xylength);
                phi = (float) Math.atan2(vy, vx);

                hue = ( (phi + piDivTwo + twoPi) % twoPi) / twoPi;
                if (pS < .001) {
                    pS = .001f;
                }
                sat = (float) ( (Math.sin(pS * theta)) / (Math.sin(pS * piDivTwo)));

                Color c = Color.getHSBColor(hue, sat, 1f);
                red = c.getRed();
                green = c.getGreen();
                blue = c.getBlue();

                // normalize red,green,blue to between 0 and 1
                r = red / 255f;
                g = green / 255f;
                b = blue / 255f;

                // shift blue
                if (r != 0 && g != 0 && b != 0) {
                    blueShiftColors = shiftBlue(r, g, b);
                    r = blueShiftColors[0];
                    g = blueShiftColors[1];
                    b = blueShiftColors[2];
                }

                // shift red
                if (r != 0 && g != 0 && b != 0) {
                    redShiftColors = shiftRed(r, g, b);
                    r = redShiftColors[0];
                    g = redShiftColors[1];
                    b = redShiftColors[2];
                }

                // adjust green
                if (r != 0 && g != 0 && b != 0) {
                    greenAdjColors = adjustGreen(r, g, b);
                    r = greenAdjColors[0];
                    g = greenAdjColors[1];
                    b = greenAdjColors[2];
                }

                // truncate
                truncMultColors = truncateRGB(r, g, b, clippedBuffer[j]);
                r = truncMultColors[0];
                g = truncMultColors[1];
                b = truncMultColors[2];

                // gamma correction
                r = (float) Math.pow(r, (1 / gamma));
                g = (float) Math.pow(g, (1 / gamma));
                b = (float) Math.pow(b, (1 / gamma));

                // now get r,g,b to 0 - 255 range
                red = Math.round(r * 255);
                green = Math.round(g * 255);
                blue = Math.round(b * 255);
            }

            // now do the orring
            val = 0xff000000 | (red << 16) | (green << 8) | (blue);

            // now put replace in cleanImageBufferA
            index = i / 4;
            cleanImageBufferA[index] = val;
        }
    }

    /**
     * rotationsl symmetry
     * 
     * @param zSlice
     * @param buff
     */
    public void rotationalSymm(int zSlice, float[] buff) {
        float vx, vy, vz;
        float hue, sat;
        float theta, phi;
        float xylength;
        int val, index;
        int red, green, blue;
        float r, g, b;
        for (int i = 0, j = (zSlice * buff.length) / 4; i <= (buff.length - 4); i = i + 4, j++) {
            vx = buff[i + 1];
            vy = buff[i + 2];
            vz = buff[i + 3];
            if (vx == 0 && vy == 0 && vz == 0) {
                red = 0;
                green = 0;
                blue = 0;
            } else {
                if (vz < 0) {
                    vx = -vx;
                    vy = -vy;
                    vz = -vz;
                }
                xylength = (float) Math.sqrt( (vx * vx) + (vy * vy));

                theta = (float) Math.asin(xylength);
                phi = (float) Math.atan2(vy, vx);

                hue = ( (2 * (phi - piDivTwo + twoPi)) % twoPi) / twoPi;
                if (pS < .001) {
                    pS = .001f;
                }
                sat = (float) ( (Math.sin(pS * theta)) / (Math.sin(pS * piDivTwo)));
                Color c = Color.getHSBColor(hue, sat, 1f);
                red = c.getRed();
                green = c.getGreen();
                blue = c.getBlue();

                // normalize red,green,blue to between 0 and 1
                r = red / 255f;
                g = green / 255f;
                b = blue / 255f;

                // shift blue
                if (r != 0 && g != 0 && b != 0) {
                    blueShiftColors = shiftBlue(r, g, b);
                    r = blueShiftColors[0];
                    g = blueShiftColors[1];
                    b = blueShiftColors[2];
                }

                // shift red
                if (r != 0 && g != 0 && b != 0) {
                    redShiftColors = shiftRed(r, g, b);
                    r = redShiftColors[0];
                    g = redShiftColors[1];
                    b = redShiftColors[2];
                }

                // adjust green
                if (r != 0 && g != 0 && b != 0) {
                    greenAdjColors = adjustGreen(r, g, b);
                    r = greenAdjColors[0];
                    g = greenAdjColors[1];
                    b = greenAdjColors[2];
                }

                // truncate
                truncMultColors = truncateRGB(r, g, b, clippedBuffer[j]);
                r = truncMultColors[0];
                g = truncMultColors[1];
                b = truncMultColors[2];

                // gamma correction
                r = (float) Math.pow(r, (1 / gamma));
                g = (float) Math.pow(g, (1 / gamma));
                b = (float) Math.pow(b, (1 / gamma));

                // now get r,g,b to 0 - 255 range
                red = Math.round(r * 255);
                green = Math.round(g * 255);
                blue = Math.round(b * 255);
            }

            // now do the orring
            val = 0xff000000 | (red << 16) | (green << 8) | (blue);

            // now put replace in cleanImageBufferA
            index = i / 4;
            cleanImageBufferA[index] = val;
        }
    }

    /**
     * mirror symmetry
     * 
     * @param zSlice
     * @param buff
     */
    public void mirrorSymm(int zSlice, float[] buff) {
        float vx, vy, vz;
        float hue, sat;
        float theta, phi, phi_deg;
        float xylength;
        int val, index;
        int red, green, blue;
        float r, g, b;
        for (int i = 0, j = (zSlice * buff.length) / 4; i <= (buff.length - 4); i = i + 4, j++) {
            vx = buff[i + 1];
            vy = buff[i + 2];
            vz = buff[i + 3];
            if (vx == 0 && vy == 0 && vz == 0) {
                red = 0;
                green = 0;
                blue = 0;
            } else {
                if (vz < 0) {
                    // vx = -vx;
                    vy = -vy;
                    vz = -vz;
                }
                vx = Math.abs(vx);
                xylength = (float) Math.sqrt( (vx * vx) + (vy * vy));

                theta = (float) Math.asin(xylength);

                phi = (float) Math.atan2(vy, vx);
                phi_deg = (float) Math.toDegrees(phi);

                hue = 720 - 2 * ( (phi_deg + 45f + 180f) % 180);
                if (hue >= 360) {
                    hue = hue - 360;
                }
                hue = hue / 360f;
                if (pS < .001) {
                    pS = .001f;
                }
                sat = (float) ( (Math.sin(pS * theta)) / (Math.sin(pS * piDivTwo)));
                Color c = Color.getHSBColor(hue, sat, 1f);
                red = c.getRed();
                green = c.getGreen();
                blue = c.getBlue();

                // normalize red,green,blue to between 0 and 1
                r = red / 255f;
                g = green / 255f;
                b = blue / 255f;

                // shift blue
                if (r != 0 && g != 0 && b != 0) {
                    blueShiftColors = shiftBlue(r, g, b);
                    r = blueShiftColors[0];
                    g = blueShiftColors[1];
                    b = blueShiftColors[2];
                }

                // shift red
                if (r != 0 && g != 0 && b != 0) {
                    redShiftColors = shiftRed(r, g, b);
                    r = redShiftColors[0];
                    g = redShiftColors[1];
                    b = redShiftColors[2];
                }

                // adjust green
                if (r != 0 && g != 0 && b != 0) {
                    greenAdjColors = adjustGreen(r, g, b);
                    r = greenAdjColors[0];
                    g = greenAdjColors[1];
                    b = greenAdjColors[2];
                }

                // truncate
                truncMultColors = truncateRGB(r, g, b, clippedBuffer[j]);
                r = truncMultColors[0];
                g = truncMultColors[1];
                b = truncMultColors[2];

                // gamma correction
                r = (float) Math.pow(r, (1 / gamma));
                g = (float) Math.pow(g, (1 / gamma));
                b = (float) Math.pow(b, (1 / gamma));

                // now get r,g,b to 0 - 255 range
                red = Math.round(r * 255);
                green = Math.round(g * 255);
                blue = Math.round(b * 255);
            }

            // now do the orring
            val = 0xff000000 | (red << 16) | (green << 8) | (blue);

            // now put replace in cleanImageBufferA
            index = i / 4;
            cleanImageBufferA[index] = val;
        }
    }

    /**
     * clipAnisotropyBuffer
     * 
     */
    public void clipAnisotropyBuffer() {
        for (int i = 0; i < anisotropyBuffer.length; i++) {
            float temp = anisotropyBuffer[i];
            temp = (temp - anisotropyMin) / (anisotropyMax - anisotropyMin);
            if (temp > 1) {
                temp = 1;
            } else if (temp < 0) {
                temp = 0;
            }
            clippedBuffer[i] = temp;
        }
    }

    /**
     * truncat RGB based on heuristic parameters
     * 
     * @param r1
     * @param g1
     * @param b1
     * @param scale
     * @return
     */
    public float[] truncateRGB(float r1, float g1, float b1, float scale) {
        float colors[] = new float[3];
        float exponent = adjustExp / stevensBeta;
        float value = (float) Math.pow(scale, exponent);
        if (isMultiply) {
            r1 = r1 * value;
            g1 = g1 * value;
            b1 = b1 * value;
        } else {
            if (scale < 0.000001f) {
                r1 = 0;
                g1 = 0;
                b1 = 0;
            }
        }
        colors[0] = r1;
        colors[1] = g1;
        colors[2] = b1;

        return colors;
    }

    /**
     * blue shift
     * 
     * @param r1
     * @param g1
     * @param b1
     * @return
     */
    public float[] shiftBlue(float r1, float g1, float b1) {
        float colors[] = new float[3];

        float b = b1 / (r1 + g1 + b1);
        float cB = Math.max( (3 / 2f) * pB * (b - (1 / 3f)) * pC, 0);
        float rS = (cB * b1) + ( (1 - cB) * r1);
        float gS = (cB * b1) + ( (1 - cB) * g1);
        float bS = b1;

        colors[0] = rS;
        colors[1] = gS;
        colors[2] = bS;

        return colors;
    }

    /**
     * red shift
     * 
     * @param r1
     * @param g1
     * @param b1
     * @return
     */
    public float[] shiftRed(float r1, float g1, float b1) {
        float colors[] = new float[3];
        float pR = pB / 4f;

        float b = b1 / (r1 + g1 + b1);

        float cB = Math.max( (3 / 2f) * pR * (b - (1 / 3f)) * pC, 0);

        float rS = (cB * b1) + ( (1 - cB) * r1);
        float gS = (cB * b1) + ( (1 - cB) * g1);
        float bS = b1;

        colors[0] = rS;
        colors[1] = gS;
        colors[2] = bS;

        return colors;
    }

    /**
     * adjust green intensity
     * 
     * @param r1
     * @param g1
     * @param b1
     * @return
     */
    public float[] adjustGreen(float r1, float g1, float b1) {
        float colors[] = new float[3];

        float max1 = Math.max(r1, g1);
        float max2 = Math.max(max1, b1);
        float maxVal = Math.max(max2, .0000001f);
        r1 = r1 / maxVal;
        g1 = g1 / maxVal;
        b1 = b1 / maxVal;
        float thrd = 1 / 3f;
        float c1 = thrd - (pG / 25f);
        float c2 = thrd + (pG / 4f);
        float leql = 0.7f;
        float totalVal = (float) ( ( (c1 * r1) + (c2 * g1) + ( (1 - c2 - stevensBeta) * b1)) / Math.pow(leql,
                (1 / stevensBeta)));
        if (totalVal < 1) {
            totalVal = 1;
        }
        r1 = r1 / (pC * totalVal + (1 - pC));
        g1 = g1 / (pC * totalVal + (1 - pC));
        b1 = b1 / (pC * totalVal + (1 - pC));

        colors[0] = r1;
        colors[1] = g1;
        colors[2] = b1;
        return colors;
    }

    /**
     * mouse clicked
     */
    public void mouseClicked(MouseEvent mouseEvent) {

    }

    /**
     * mouse dragged
     */
    public void mouseDragged(MouseEvent mouseEvent) {

    }

    /**
     * mouse entered
     */
    public void mouseEntered(MouseEvent mouseEvent) {

    }

    /**
     * mouse exited
     */
    public void mouseExited(MouseEvent mouseEvent) {

    }

    /**
     * mouse moved
     */
    public void mouseMoved(MouseEvent mouseEvent) {

    }

    /**
     * mouse pressed
     */
    public void mousePressed(MouseEvent mouseEvent) {

    }

    /**
     * mouse pressed paint
     */
    protected void mousePressedPaint(MouseEvent mouseEvent) {

    }

    /**
     * mouse released
     */
    public void mouseReleased(MouseEvent mouseEvent) {

    }

    /**
     * mouse wheel moved
     */
    public void mouseWheelMoved(MouseWheelEvent mouseWheelEvent) {

    }

}
