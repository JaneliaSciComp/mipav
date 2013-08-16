package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

public class AlgorithmEdgeDetection3D extends AlgorithmBase {
    
    // Reference: Directional 3D Edge Detection in Anisotropic Data: Detector Design and Performance Assessment
    // by Marek Brejl and Milan Sonka
    
    // Size of the prism over which the data are integrated during the acquisition process
    private double dx;
    private double dy;
    private double dz;
    // Mask size
    private int nx = 5;
    private int ny = 5;
    private int nz = 5;
    // Neighborhood size for the directional gradient averaging L (recommended empirically derived value
    // for the mask size of 5 x 5 x 5 = 2.1)
    private double L = 2.1;
    double threshold = 0.12;
    private boolean componentsRequired = false;
    private boolean anglesRequired = false;
    private boolean magnitudeRequired = false;
    
    private double FX[][][];
    private double FY[][][];
    private double FZ[][][];
    
    private double thetaMAX[][][];
    private double phiMAX[][][];
    private double FMAX[][][];
    
  //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmEdgeDetection3D - default constructor.
     */
    public AlgorithmEdgeDetection3D() { }
    
    /**
     * AlgorithmEdgeDetection3D.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     * @param  dx
     * @param  dy
     * @param  dz
     * @param  nx
     * @param  ny
     * @param  nz
     * @param  L
     * @param  threshold
     * @param  componentsRequired;
     * @param  anglesRequired;
     * @param  magnitudeRequired
     */
    public AlgorithmEdgeDetection3D(ModelImage destImg, ModelImage srcImg, double dx, double dy, double dz,
            int nx, int ny, int nz, double L, double threshold, boolean componentsRequired, boolean anglesRequired,
            boolean magnitudeRequired) {
        super(destImg, srcImg);
        this.dx = dx;
        this.dy = dy;
        this.dz = dz;
        this.nx = nx;
        this.ny = ny;
        this.nz = nz;
        this.L = L;
        this.threshold = threshold;
        this.componentsRequired = componentsRequired;
        this.anglesRequired = anglesRequired;
        this.magnitudeRequired = magnitudeRequired;
    }
    
  //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        int xDim;
        int yDim;
        int zDim;
        float vx;
        float vy;
        float vz;
        int xl;
        int xu;
        int yl;
        int yu;
        int zl;
        int zu;
        int xin;
        int yin;
        int zin;
        double X0 = 0.0;
        double X2 = 0.0;
        double X31 = 0.0;
        double X32 = 0.0;
        double X21 = 0.0;
        double X22 = 0.0;
        double Y0 = 0.0;
        double Y2 = 0.0;
        double Y31 = 0.0;
        double Y32 = 0.0;
        double Y21 = 0.0;
        double Y22 = 0.0;
        double Z0 = 0.0;
        double Z2 = 0.0;
        double Z31 = 0.0;
        double Z32 = 0.0;
        double Z21 = 0.0;
        double Z22 = 0.0;
        double x;
        double y;
        double z;
        double dx2 = dx * dx;
        double dy2 = dy * dy;
        double dz2 = dz * dz;
        double x2;
        double y2;
        double z2;
        double xg1;
        double xg2;
        double yg1;
        double yg2;
        double zg1;
        double zg2;
        double U2;
        double V2;
        double W2;
        double A2;
        double U3;
        double V3;
        double W3;
        double A3;
        double U4;
        double V4;
        double W4;
        double A4;
        double MX[][][] = new double[nx][ny][nz];
        double MY[][][] = new double[nx][ny][nz];
        double MZ[][][] = new double[nx][ny][nz];
        double MXLast;
        double MYLast;
        double MZLast;
        double L2 = L * L;
        int zin2;
        int yin2;
        int xin2;
        int sliceSize;
        int length;
        double buffer[];
        double FX2Y2;
        double fx;
        double fy;
        double fz;
        double largestFMAX;
        byte edgeBuffer[];
        
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running 3D Edge Detection ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        vx = srcImage.getFileInfo(0).getResolutions()[0];
        vy = srcImage.getFileInfo(0).getResolutions()[1];
        vz = srcImage.getFileInfo(0).getResolutions()[2];
        xu = (nx - 1)/2;
        xl = -xu;
        yu = (ny - 1)/2;
        yl = -yu;
        zu = (nz - 1)/2;
        zl = -zu;
        sliceSize = xDim * yDim;
        length = sliceSize * zDim;
        buffer = new double[length];
        try {
            srcImage.exportData(0, length, buffer);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData(0, length, buffer)");
            setCompleted(false);
            return;
        }
        
        for (xin = xl; xin <= xu; xin++) {
            x = xin * vx;
            x2 = x * x;
            xg1 = (x2 + dx2/3.0);
            xg2 = (x2 + dx2);
            X0 += 1.0;
            X2 += x2;
            X31 += xg1;
            X32 += (xg1 * xg1);
            X21 += (x2 * xg2);
            X22 += (x2 * xg2 * xg2);
        }
        
        for (yin = yl; yin <= yu; yin++) {
            y = yin * vy;
            y2 = y * y;
            yg1 = (y2 + dy2/3.0);
            yg2 = (y2 + dy2);
            Y0 += 1.0;
            Y2 += y2;
            Y31 += yg1;
            Y32 += (yg1 * yg1);
            Y21 += (y2 * yg2);
            Y22 += (y2 * yg2 * yg2);
        }
        
        for (zin = zl; zin <= zu; zin++) {
            z = zin * vz;
            z2 = z * z;
            zg1 = (z2 + dz2/3.0);
            zg2 = (z2 + dz2);
            Z0 += 1.0;
            Z2 += z2;
            Z31 += zg1;
            Z32 += (zg1 * zg1);
            Z21 += (z2 * zg2);
            Z22 += (z2 * zg2 * zg2);
        }
        
        U2 = Y0 * Z0 * (X2 * X22 - X21 * X21);
        V2 = X2 * Z0 * (Y0 * Y32 - Y31 * Y31);
        W2 = X2 * Y0 * (Z0 * Z32 - Z31 * Z31);
        A2 = (1.0/(X2 * Y0 * Z0)) * (1.0 - X21*X21/(X21*X21 - X2*X22) - Y31*Y31/(Y31*Y31 - Y0*Y32) - Z31*Z31/(Z31*Z31 - Z0*Z32));
        
        U3 = Y2 * Z0 * (X0 * X32 - X31 * X31);
        V3 = X0 * Y2 * (Z0 * Z32 - Z31 * Z31);
        W3 = X0 * Z0 * (Y2 * Y22 - Y21 * Y21);
        A3 = (1.0/(X0 * Y2 * Z0)) * (1.0 - X31*X31/(X31*X31 - X0*X32) - Y21*Y21/(Y21*Y21 - Y2*Y22) - Z31*Z31/(Z31*Z31 - Z0*Z32));
        
        U4 = Y0 * Z2 * (X0 * X32 - X31 * X31);
        V4 = X0 * Z2 * (Y0 * Y32 - Y31 * Y31);
        W4 = X0 * Y0 * (Z2 * Z22 - Z21 * Z21);
        A4 = (1.0/(X0 * Y0 * Z2)) * (1.0 - X31*X31/(X31*X31 - X0*X32) - Y31*Y31/(Y31*Y31 - Y0*Y32) - Z21*Z21/(Z21*Z21 - Z2*Z22));
        
        for (xin = xl; xin <= xu; xin++) {
            x = xin * vx;
            x2 = x * x;
            xg2 = x2 + dx2;
            MXLast = (1.0/U2)*L2*(xg2 * X2 - X21)*x;
            for (yin = yl; yin <= yu; yin++) {
                y = yin * vy;
                y2 = y * y;
                yg1 = y2 + dy2/3.0;
                for (zin = zl; zin <= zu; zin++) {
                    z = zin * vz;
                    z2 = z * z;
                    zg1 = z2 + dz2/3.0;
                    MX[xin - xl][yin - yl][zin - zl] = (A2 - (1.0/U2)*X21*xg2 - (1.0/V2)*Y31*yg1 - (1.0/W2)*Z31*zg1)*x +
                    (1.0/(3.0*V2))*L2*(yg1*Y0 - Y31)*x + (1.0/(3.0*W2))*L2*(zg1*Z0 - Z31)*x + MXLast;
                }
            }
        }
        
        for (yin = yl; yin <= yu; yin++) {
            y = yin * vy;
            y2 = y * y;
            yg2 = y2 + dy2;
            MYLast = (1.0/W3)*L2*(yg2*Y2 - Y21)*y;
            for (xin = xl; xin <= xu; xin++) {
                x = xin * vx;
                x2 = x * x;
                xg1 = x2 + dx2/3.0;
                for (zin = zl; zin <= zu; zin++) {
                    z = zin * vz;
                    z2 = z * z;
                    zg1 = z2 + dz2/3.0;
                    MY[xin - xl][yin - yl][zin - zl] = (A3 - (1.0/U3)*X31*xg1 - (1.0/W3)*Y21*yg2 - (1.0/V3)*Z31*zg1)*y +
                    (1.0/(3.0*U3))*L2*(xg1*X0 - X31)*y + (1.0/(3.0*V3))*L2*(zg1*Z0 - Z31)*y + MYLast;
                }
            }
        }
        
        for (zin = zl; zin <= zu; zin++) {
            z = zin * vz;
            z2 = z * z;
            zg2 = z2 + dz2;
            MZLast = (1.0/W4)*L2*(zg2*Z2 - Z21)*z;
            for (xin = xl; xin <= xu; xin++) {
                x = xin * vx;
                x2 = x * x;
                xg1 = x2 + dx2/3.0;
                for (yin = yl; yin <= yu; yin++) {
                    y = yin * vy;
                    y2 = y * y;
                    yg1 = y2 + dy2/3.0;
                    MZ[xin - xl][yin - yl][zin - zl] = (A4 - (1.0/U4)*X31*xg1 - (1.0/V4)*Y31*yg1 - (1.0/W4)*Z21*zg2)*z +
                    (1.0/(3.0*U4))*L2*(xg1*X0 - X31)*z + (1.0/(3.0*V4))*L2*(yg1*Y0 - Y31)*z + MZLast;
                }
            }
        }
        
        if (componentsRequired) {
            FX = new double[xDim][yDim][zDim];
            FY = new double[xDim][yDim][zDim];
            FZ = new double[xDim][yDim][zDim];
        }
        if (anglesRequired) {
            thetaMAX = new double[xDim][yDim][zDim];
            phiMAX = new double[xDim][yDim][zDim];
        }
        FMAX = new double[xDim][yDim][zDim];
        largestFMAX = 0.0;
        for (zin = zu; zin <= zDim - 1 - zu; zin++) {
            for (yin = yu; yin <= yDim - 1 - yu; yin++) {
                for (xin = xu; xin <= xDim - 1 - xu; xin++) {
                    fx = 0.0;
                    fy = 0.0;
                    fz = 0.0;
                    for (zin2 = zl; zin2 <= zu; zin2++) {
                        for (yin2 = yl; yin2 <= yu; yin2++) {
                            for (xin2 = xl; xin2 <= xu; xin2++) {
                                fx += MX[xin2-xl][yin2-yl][zin2-zl]*buffer[xin + xin2 + (yin + yin2)*xDim + (zin + zin2)*sliceSize];
                                fy += MY[xin2-xl][yin2-yl][zin2-zl]*buffer[xin + xin2 + (yin + yin2)*xDim + (zin + zin2)*sliceSize];
                                fz += MZ[xin2-xl][yin2-yl][zin2-zl]*buffer[xin + xin2 + (yin + yin2)*xDim + (zin + zin2)*sliceSize];
                            }
                        }
                    }
                    if (componentsRequired) {
                        FX[xin][yin][zin] = fx;
                        FY[xin][yin][zin] = fy;
                        FZ[xin][yin][zin] = fz;
                    }
                    FX2Y2 = fx*fx + fy*fy;
                    if (anglesRequired) {
                        thetaMAX[xin][yin][zin] = (180.0/Math.PI)*Math.atan2(fy, fx);
                        phiMAX[xin][yin][zin] = (180./Math.PI)*Math.atan2(fz, Math.sqrt(FX2Y2));
                    }
                    FMAX[xin][yin][zin] = Math.sqrt(FX2Y2 + fz * fz);
                    if (FMAX[xin][yin][zin] > largestFMAX) {
                        largestFMAX = FMAX[xin][yin][zin];
                    }
                }
            }
        }
        
        edgeBuffer = new byte[length];
        for (zin = zu; zin <= zDim - 1 - zu; zin++) {
            for (yin = yu; yin <= yDim - 1 - yu; yin++) {
                for (xin = xu; xin <= xDim - 1 - xu; xin++) {
                    if ((FMAX[xin][yin][zin]/largestFMAX) >= threshold) {
                        edgeBuffer[xin + yin * xDim + zin * sliceSize] = 1;
                    }
                }
            }
        }
        
        if (!magnitudeRequired) {
            for (xin = 0; xin < xDim; xin++) {
                for (yin = 0; yin < yDim; yin++) {
                    FMAX[xin][yin] = null;
                }
            }
            for (xin = 0; xin < xDim; xin++) {
                FMAX[xin] = null;
            }
            FMAX = null;
        }
        
        try {
            destImage.importData(0, edgeBuffer, true);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.importData(0, edgeBuffer, true)");
            setCompleted(false);
            return;
        }
        
        setCompleted(true);
        return;
    }
    
    public double[][][] getXComponent() {
        return FX;
    }
    
    public double[][][] getYComponent() {
        return FY;    
    }
    
    public double[][][] getZComponent() {
        return FZ;
    }
    
    public double[][][] getTheta() {
        return thetaMAX;
    }
    
    public double[][][] getPhi() {
        return phiMAX;
    }
    
    public double[][][] getMagnitude() {
        return FMAX;
    }
}