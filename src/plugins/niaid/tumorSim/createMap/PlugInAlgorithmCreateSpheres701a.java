package niaid.tumorSim.createMap;


// MIPAV is freely available from http://mipav.cit.nih.gov

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.

/*****************************************************************
 ****************************************************************** 
 The MIPAV application is intended for research use only. This application has NOT been approved for ANY diagnostic
 * use by the Food and Drug Administration. There is currently no approval process pending.
 * 
 * This software may NOT be used for diagnostic purposes.
 * 
 ****************************************************************** 
 ******************************************************************/

import gov.nih.mipav.util.CircleUtil;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtraction;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmNoise;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase.DataType;

import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogSubsample;

import java.awt.event.ActionEvent;
import java.text.DecimalFormat;
import java.util.Random;

import niaid.tumorSim.createMap.PlugInDialogCreateSpheres701a.NoiseMode;
import WildMagic.LibFoundation.Mathematics.Vector3d;


/**
 * This class implements a basic algorithm that performs operations on 2D and 3D images. By extending AlgorithmBase, it
 * has no more functionality than any other algorithm in MIPAV. No functionality specifically makes it a plug-in.
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */

public class PlugInAlgorithmCreateSpheres701a extends AlgorithmBase {

    public static final String INTENSITY1 = "Intensity Pre: ";

    public static final String INTENSITY2 = "Intensity Post: ";

    public static final String RADIUS1 = "Radius Pre: ";

    public static final String RADIUS2 = "Radius Post: ";

    public static final String STD_DEV = "std dev: ";

    public static final String NOISE_LEVEL = "Adding noise parameter: ";

    public static final String NORMAL_TISSUE = "Normal tissue: ";

    public static final String PARTIAL = "Partial volume pixels: ";

    public static final String TOTAL = "Total voxels in tumor";

    /** Image dimensions */
    private int xyDim, zDim;

    /** Image resolutions */
    private double xyRes, zRes;

    /** Tumor radius in units of image */
    private double initRadius;

    /** Percent tumor change, usually 0 to simulate treatment options */
    private double tumorChange;

    /** SimMode (decay, increase, no change), usually no change */
    private PlugInDialogCreateSpheres701a.SphereSimMode simMode;

    /** Center of created sphere */
    private int xCenter, yCenter, zCenter;

    /** Tumor intensities for image1 and image2 */
    private double tumorIntensity1, tumorIntensity2;

    /** Pixels for sphere location */
    private int[][] sphere;

    /** Amount of subsampling to occur */
    private int subsampleAmount;

    /** Whether boundary checking is necessary during sphere population */
    private boolean doBoundCheck;

    /** Whether sphere should be populated entirely within field of view */
    private boolean doCenter;

    /** Maximum of Rician distributed noise */
    private double noiseMax;

    /** Standard deviation of Gaussian distributed noise */
    private double stdDevGaussian;

    /** Normal tissue value, assummed same for image 1 and 2 */
    private double normalTissue;

    /** Standard deviation of normal tissue intensity and tumor intensities */
    private double stdDevTumorIntensity1, stdDevTumorIntensity2, stdDevNormal;

    /** Noise mode */
    private NoiseMode noise;

    /** Alg iteration number */
    private int iter = -1;

    public PlugInAlgorithmCreateSpheres701a() {
        // TODO Auto-generated constructor stub
    }

    /**
     * Constructor.
     * 
     * @param tumorIntensity1
     * @param tumorIntensity2
     * @param stdDevTumorIntensity2
     * @param noise
     * @param intensity22
     * @param noiseParam either rician or gaussian noise parameter
     * @param normalTissue
     * @param stdDevNormal
     * @param subsample
     * 
     */
    public PlugInAlgorithmCreateSpheres701a(final int xyDim, final int zDim, final double xyRes, final double zRes, final double initRadius,
            double tumorChange, final PlugInDialogCreateSpheres701a.SphereSimMode simMode, final double tumorIntensity1, final double stdDevTumorIntensity1,
            final double tumorIntensity2, final double stdDevTumorIntensity2, final int subsampleAmount, final boolean doCenter, final NoiseMode noise,
            final double noiseParam, final double normalTissue, final double stdDevNormal) {
        this.xyDim = xyDim;
        this.zDim = zDim;

        this.xyRes = xyRes;
        this.zRes = zRes;

        this.doCenter = doCenter;

        this.initRadius = initRadius;
        if (tumorChange >= 2) {
            tumorChange /= 100;
        } else if (tumorChange >= 1) {
            tumorChange = tumorChange - 1;
        }
        this.tumorChange = tumorChange;
        this.simMode = simMode;
        this.tumorIntensity1 = tumorIntensity1;
        this.stdDevTumorIntensity1 = stdDevTumorIntensity1;
        this.tumorIntensity2 = tumorIntensity2;
        this.stdDevTumorIntensity2 = stdDevTumorIntensity2;

        this.subsampleAmount = subsampleAmount;

        this.noise = noise;

        switch (noise) {
            case gaussian:
                this.stdDevGaussian = noiseParam;
                break;
            default:
                this.noiseMax = noiseParam;
                break;
        }

        this.normalTissue = normalTissue;
        this.stdDevNormal = stdDevNormal;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Starts the algorithm. At the conclusion of this method, AlgorithmBase reports to any algorithm listeners that
     * this algorithm has completed. This method is not usually called explicitly by a controlling dialog. Instead, see
     * AlgorithmBase.run() or start().
     */
    @Override
    public void runAlgorithm() {
        final FileInfoImageXML fileInfoImage1 = new FileInfoImageXML("image1a", null, FileUtility.RAW);
        final FileInfoImageXML fileInfoImage2 = new FileInfoImageXML("image2a", null, FileUtility.RAW);

        final FileInfoImageXML fileInfoImage1TumorOnly = new FileInfoImageXML("image1a_tumor", null, FileUtility.RAW);
        final FileInfoImageXML fileInfoImage2TumorOnly = new FileInfoImageXML("image2a_tumor", null, FileUtility.RAW);

        if (iter != -1) {
            fileInfoImage1.setFileName(fileInfoImage1.getFileName() + "_iter" + iter);
            fileInfoImage1TumorOnly.setFileName(fileInfoImage1TumorOnly.getFileName() + "_iter" + iter);
            fileInfoImage2.setFileName(fileInfoImage2.getFileName() + "_iter" + iter);
            fileInfoImage2TumorOnly.setFileName(fileInfoImage2TumorOnly.getFileName() + "_iter" + iter);
        }

        setBasicInfo(fileInfoImage1, DataType.FLOAT);
        setBasicInfo(fileInfoImage2, DataType.FLOAT);

        setBasicInfo(fileInfoImage1TumorOnly, DataType.FLOAT);
        setBasicInfo(fileInfoImage2TumorOnly, DataType.FLOAT);

        ModelImage image1a = new ModelImage(DataType.DOUBLE.getLegacyNum(), new int[] {xyDim, xyDim, zDim}, "image1a");
        ModelImage image2a = new ModelImage(DataType.DOUBLE.getLegacyNum(), new int[] {xyDim, xyDim, zDim}, "image2a");

        ModelImage image1aTumor = new ModelImage(DataType.DOUBLE.getLegacyNum(), new int[] {xyDim, xyDim, zDim}, "image1a_tumor");
        ModelImage image2aTumor = new ModelImage(DataType.DOUBLE.getLegacyNum(), new int[] {xyDim, xyDim, zDim}, "image2a_tumor");

        if (iter != -1) {
            image1a.setImageName(image1a.getImageName() + "_iter" + iter);
            image1aTumor.setImageName(image1aTumor.getImageName() + "_iter" + iter);
            image2a.setImageName(image2a.getImageName() + "_iter" + iter);
            image2aTumor.setImageName(image2aTumor.getImageName() + "_iter" + iter);
        }

        setNormalTissue(image1a, stdDevNormal);
        setNormalTissue(image2a, stdDevNormal);

        final int xyLargerRadius = (int) Math.ceil(defineLargerRadius() / xyRes);

        doBoundCheck = false;
        final Random r = new Random();
        if ( !doCenter || (xyDim - 2 * xyLargerRadius) <= 0) {
            xCenter = r.nextInt(xyDim);
            yCenter = r.nextInt(xyDim);
            doBoundCheck = true;
            if ( (xyDim - 1 - 2 * xyLargerRadius) < 0) {
                Preferences.data("Unable to enclose tumor within FOV due to large radius. \n");
            }
        } else { // ensures entire tumor is inside image
            xCenter = r.nextInt(xyDim - 1 - 2 * xyLargerRadius) + xyLargerRadius;
            yCenter = r.nextInt(xyDim - 1 - 2 * xyLargerRadius) + xyLargerRadius;
        }

        final int zLargerRadius = (int) Math.ceil(defineLargerRadius() / zRes);

        if ( !doCenter || (zDim - 1 - 2 * zLargerRadius) <= 0) {
            zCenter = r.nextInt(zDim);
        } else {
            zCenter = r.nextInt(zDim - 1 - 2 * zLargerRadius) + zLargerRadius;
            doBoundCheck = true;
        }

        Preferences.debug("Center of tumor: " + xCenter + ", " + yCenter + ", " + zCenter + "\n");
        Preferences.data("Center of tumor: " + xCenter + ", " + yCenter + ", " + zCenter + "\n");

        double stdTumor1Display = stdDevTumorIntensity1;
        double stdTumor2Display = stdDevTumorIntensity2;
        double stdDevNormalDisplay = stdDevNormal;
        if (subsampleAmount != 0) {
            stdTumor1Display = stdTumor1Display * PlugInDialogCreateSpheres701a.gaussAvgFactor;
            stdTumor2Display = stdTumor2Display * PlugInDialogCreateSpheres701a.gaussAvgFactor;
            stdDevNormalDisplay = stdDevNormalDisplay * PlugInDialogCreateSpheres701a.gaussAvgFactor;
        }

        Preferences.data(INTENSITY1 + tumorIntensity1 + "\t" + STD_DEV + stdTumor1Display + ";\n");
        Preferences.data(INTENSITY2 + tumorIntensity2 + "\t" + STD_DEV + stdTumor2Display + ";\n");
        Preferences.data(NORMAL_TISSUE + normalTissue + "\t" + STD_DEV + stdDevNormalDisplay + ";\n");

        fireProgressStateChanged("Populating spheres");

        populateSphere(initRadius, tumorIntensity1, stdDevTumorIntensity1, image1a);
        populateSphere(getChangedRadius(), tumorIntensity2, stdDevTumorIntensity2, image2a);

        populateSphere(initRadius, tumorIntensity1, 0, image1aTumor); // use to find subsampling effect
        populateSphere(getChangedRadius(), tumorIntensity2, 0, image2aTumor); // use to find subsampling effect

        addPartialVoluming(initRadius, image1a, image1aTumor);
        addPartialVoluming(getChangedRadius(), image2a, image2aTumor);

        if (subsampleAmount != 0) {
            image1a = subsample(image1a);
            image2a = subsample(image2a);
            image1aTumor = subsample(image1aTumor);
            image2aTumor = subsample(image2aTumor);
            image1aTumor.setImageName("image1aTumor");
            image2aTumor.setImageName("image2aTumor");
            image1a.getParentFrame().setVisible(false);
            image2a.getParentFrame().setVisible(false);
            image1aTumor.getParentFrame().setVisible(false);
            image2aTumor.getParentFrame().setVisible(false);
        }

        createVOI(image1aTumor, 0 + Double.MIN_VALUE, Double.MAX_VALUE);
        createVOI(image2aTumor, 0 + Double.MIN_VALUE, Double.MAX_VALUE);

        image1a.setImageName("image1a");
        for (int i = 0; i < image1a.getFileInfo().length; i++) {
            image1a.getFileInfo(i).setFileDirectory(Preferences.getPreferencesDir());
        }
        image2a.setImageName("image2a");
        for (int i = 0; i < image2a.getFileInfo().length; i++) {
            image2a.getFileInfo(i).setFileDirectory(Preferences.getPreferencesDir());
        }

        if (subsampleAmount != 0) {
            xCenter = (int) Math.round( ((double) xCenter) / subsampleAmount);
            yCenter = (int) Math.round( ((double) yCenter) / subsampleAmount);
            zCenter = (int) Math.round( ((double) zCenter) / subsampleAmount);
        }

        Preferences.debug("Center of tumor subsampled: " + xCenter + ", " + yCenter + ", " + zCenter + ";\n");
        Preferences.data("Center of tumor subsampled: " + xCenter + ", " + yCenter + ", " + zCenter + ";\n");

        if (noiseMax != 0 || stdDevGaussian != 0) {
            generateNoise(image1a);
            generateNoise(image2a);
        }

        countPixels(image1aTumor, tumorIntensity1, image1a);
        countPixels(image2aTumor, tumorIntensity2, image2a);

        Preferences.data(RADIUS1 + initRadius + "\n");
        Preferences.data(RADIUS2 + getChangedRadius() + "\n");

        image1a.calcMinMax();
        image2a.calcMinMax();

        image1aTumor.calcMinMax();
        image2aTumor.calcMinMax();

        if (subsampleAmount == 0) {
            final ViewJFrameImage frame1a = new ViewJFrameImage(image1a);
            final ViewJFrameImage frame2a = new ViewJFrameImage(image2a);
            final ViewJFrameImage frame1aTumor = new ViewJFrameImage(image1aTumor);
            final ViewJFrameImage frame2aTumor = new ViewJFrameImage(image2aTumor);
        }

        if (isRunningInSeparateThread()) {
            image1a.getParentFrame().setVisible(true);
            image2a.getParentFrame().setVisible(true);
            image1aTumor.getParentFrame().setVisible(true);
            image2aTumor.getParentFrame().setVisible(true);
        }

        setCompleted(true); // indicating to listeners that the algorithm completed successfully

    } // end runAlgorithm()

    /**
     * Adds partial voluming pixels to edge of sphere by multiplying edge pixels by their percent coverage of the radial
     * line from the origin of the tumor to the tumor line.
     * 
     * @param tumorImage image with normal tissue intensity == 0
     */
    private void addPartialVoluming(final double radius, final ModelImage image, final ModelImage tumorImage) {
        final Vector3d center = new Vector3d(xCenter * xyRes, yCenter * xyRes, zCenter * zRes);
        final Vector3d point = new Vector3d();
        double distance = 0.0, relevantRes = 0.0, percent = 0.0;
        for (int x = 0; x < image.getExtents()[0]; x++) {
            point.X = x * xyRes;
            for (int y = 0; y < image.getExtents()[1]; y++) {
                point.Y = y * xyRes;
                for (int z = 0; z < image.getExtents()[2]; z++) {
                    point.Z = z * zRes;
                    if (tumorImage.getDouble(x, y, z) != 0) {
                        distance = center.distance(point);
                        if (distance > radius) {
                            relevantRes = xyRes;
                            if (zCenter - z > xCenter - x || zCenter - z > yCenter - y) {
                                relevantRes = zRes;
                            }
                            percent = 1 - ( (distance - radius) / relevantRes); // tumor contribution to pixel
                            tumorImage.set(x, y, z, percent * tumorImage.getDouble(x, y, z));
                            // double old = image.getDouble(x, y, z);
                            image.set(x, y, z, percent * image.getDouble(x, y, z) + ( (1 - percent) * normalTissue));
                            // System.out.println("At: "+x+", "+y+", "+z+": "+distance+"\tvs "+radius+": "+image.getDouble(x,
                            // y, z)+"\tvs "+old);
                        }
                    }
                }
            }
        }
    }

    public static void createVOI(final ModelImage image, final double lowerBound, final double upperBound) {

        final FileInfoImageXML fileInfo = (FileInfoImageXML) image.getFileInfo()[0].clone();
        fileInfo.setDataType(DataType.BOOLEAN.getLegacyNum());

        final ModelImage imageBin = new ModelImage(DataType.BOOLEAN.getLegacyNum(), image.getFileInfo()[0].getExtents(), "image1a_tumor");
        // imageBin.getParentFrame().setVisible(false);

        double intensityValue = 0.0;
        for (int i = 0; i < image.getDataSize(); i++) {
            intensityValue = image.getDouble(i);
            if (intensityValue >= lowerBound && intensityValue <= upperBound) {
                imageBin.set(i, true);
            }
        }

        AlgorithmMorphology3D idObjectsAlgo3D;
        final int method = AlgorithmMorphology3D.ID_OBJECTS;

        idObjectsAlgo3D = new AlgorithmMorphology3D(imageBin, 0, 0, method, 0, 0, 0, 0, true);
        idObjectsAlgo3D.setMinMax(1, Integer.MAX_VALUE);
        idObjectsAlgo3D.run();
        idObjectsAlgo3D.finalize();
        idObjectsAlgo3D = null;

        imageBin.calcMinMax();
        final AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(imageBin);
        VOIExtractionAlgo.setRunningInSeparateThread(false);
        VOIExtractionAlgo.run();

        VOIExtractionAlgo.disposeLocal();

        // ViewJFrameImage imageFrame = new ViewJFrameImage(imageBin);
        // imageFrame.setVisible(true);

        if (imageBin.getVOIs().size() > 0) {
            image.registerVOI(imageBin.getVOIs().get(0));
        } else {
            System.err.println("No VOI was created, likely because no tumor pixels were present.");
        }

        imageBin.disposeLocal();
        ViewUserInterface.getReference().unRegisterImage(imageBin);
    }

    private void countPixels(final ModelImage imageTumor, final double intensity, final ModelImage imageFull) {
        int numPixelsTumor = 0;
        int numPixelsPartial = 0;
        double averageInTotalTumor = 0.0, averageInPartialTumor = 0.0;
        for (int i = 0; i < imageTumor.getDataSize(); i++) {
            if (imageTumor.getDouble(i) != 0) {
                if (imageTumor.getDouble(i) != intensity) {
                    numPixelsPartial++;
                    averageInPartialTumor += imageFull.getDouble(i);
                }
                numPixelsTumor++;
                averageInTotalTumor += imageFull.getDouble(i);
            }
        }

        averageInTotalTumor = averageInTotalTumor / numPixelsTumor;
        averageInPartialTumor = averageInPartialTumor / numPixelsPartial;

        Preferences.data("For " + imageTumor.getImageName() + ", " + TOTAL + " : " + numPixelsTumor + "\n");
        Preferences.data("For " + imageFull.getImageName() + ", average tumor intensity : " + averageInTotalTumor + "\n");
        Preferences.data("For " + imageTumor.getImageName() + ", " + PARTIAL + ": " + numPixelsPartial + "\n");
        Preferences.data("For " + imageFull.getImageName() + ", average partial volume pixels intensity : " + averageInPartialTumor + "\n");

    }

    private void setNormalTissue(final ModelImage image, final double stdDevIntensity) {
        final Random r = new Random();
        for (int i = 0; i < image.getDataSize(); i++) {
            image.set(i, normalTissue + r.nextGaussian() * stdDevIntensity);
        }
    }

    /**
     * @param iter the iter to set
     */
    public void setIter(final int iter) {
        this.iter = iter;
    }

    private void generateNoise(final ModelImage image) {
        AlgorithmNoise noiseAlg = null;
        switch (noise) {
            case gaussian:
                double noiseFactor = stdDevGaussian;
                if (subsampleAmount != 0) {
                    noiseFactor = stdDevGaussian * PlugInDialogCreateSpheres701a.gaussAvgFactor;
                }
                noiseAlg = new AlgorithmNoise(image, AlgorithmNoise.GAUSSIAN, noiseFactor * 4.0, 5, 1, 0, 1); // alg
                                                                                                              // uses
                                                                                                              // 4*std_dev
                                                                                                              // to
                                                                                                              // define
                                                                                                              // min/max
                Preferences.data(NOISE_LEVEL + noiseFactor + ";\n");
                break;
            default:
                noiseAlg = new AlgorithmNoise(image, AlgorithmNoise.RICIAN, noiseMax, 5, 1, 0, 1);
                Preferences.data(NOISE_LEVEL + noiseMax + ";\n");
                break;
        }
        noiseAlg.setRunningInSeparateThread(false);
        noiseAlg.run();
    }

    private ModelImage subsample(final ModelImage image) {
        final JDialogSubsample subsample = new JDialogSubsample(image.getParentFrame(), image);
        subsample.setVisible(false);
        subsample.setProcessIndep(false);
        subsample.setDoVOI(false);
        subsample.setQuietRunning(true);
        subsample.setSubsamplingRate(subsampleAmount);
        subsample.setSeparateThread(false);
        subsample.actionPerformed(new ActionEvent(this, 0, "OK"));

        subsample.getResultImage().getParentFrame().setVisible(false);
        image.disposeLocal();
        ViewUserInterface.getReference().unRegisterImage(image);
        return subsample.getResultImage();
    }

    private void populateSphere(final double radius, final double intensity, final double stdDev, final ModelImage image) {
        final Random r = new Random();
        sphere = CircleUtil.get3DPointsInSphere(xCenter, yCenter, zCenter, xyRes, xyRes, zRes, radius);
        final int xyDimBound = xyDim - 1;
        final int zDimBound = zDim - 1;

        for (int i = 0; i < sphere.length; i++) {
            if (doBoundCheck) {
                if (sphere[i][0] > xyDimBound || sphere[i][0] < 0 || sphere[i][1] > xyDimBound || sphere[i][1] < 0 || sphere[i][2] > zDimBound
                        || sphere[i][2] < 0) {
                    continue;
                }
            }
            image.set(sphere[i][0], sphere[i][1], sphere[i][2], intensity + r.nextGaussian() * stdDev);
        }
    }

    private double getChangedRadius() {

        double newRadius = 0;
        switch (simMode) {
            case grow:
                newRadius = (initRadius * (1 + tumorChange));
                break;

            case shrink:
                newRadius = (initRadius * (1 - tumorChange));
                break;

            case none:
                // case intensify:
                // case deintensify:
            default:
                newRadius = initRadius;

        }

        return newRadius;
    }

    // private double getChangedIntensity() {
    // double newIntensity = intensity1;
    //
    // switch(simMode) {
    // case intensify:
    // newIntensity = intensity1*(1+tumorChange);
    // break;
    // case deintensify:
    // newIntensity = intensity1*(1-tumorChange);
    // break;
    // case none:
    // case grow:
    // case shrink:
    // default:
    // newIntensity = intensity1;
    //
    // }
    //
    // return newIntensity;
    // }

    private double defineLargerRadius() {

        double largerRadius = 0;
        switch (simMode) {
            case grow:
                largerRadius = (initRadius * (1 + tumorChange));
                break;

            case shrink:
                largerRadius = initRadius;
                break;

            case none:
                // case intensify:
                // case deintensify:
            default:
                largerRadius = initRadius;

        }

        return largerRadius;
    }

    private void setBasicInfo(final FileInfoImageXML fileInfo, final DataType type) {
        fileInfo.setDataType(type.getLegacyNum());
        fileInfo.setExtents(new int[] {xyDim, xyDim, zDim});
        fileInfo.setUnitsOfMeasure(new Unit[] {Unit.MILLIMETERS, Unit.MILLIMETERS, Unit.MILLIMETERS});
        fileInfo.setResolutions(new float[] {(float) xyRes, (float) xyRes, (float) zRes});
        fileInfo.setEndianess(false);
        fileInfo.setOffset(0);
        fileInfo.setFileDirectory(Preferences.getPreferencesDir());

    }

    private class RayleighRandom extends Random {

        /**
         * Outputs next number of rayleigh distribution with given scale and minimum of 0, the use of nextNext and
         * synchronized helps thread performance and is based of java.util.Random source code.
         * 
         * @param scale
         * @param min
         * @return
         */
        synchronized public double nextRayleigh(double scale, final double min) {
            final double x = nextDouble();

            final double sub = x - min;

            scale = scale * scale;

            final double f = (sub / scale) * Math.exp( - (sub * sub) / (2 * scale * scale));

            return f;
        }

        public double gaussian(final double x, final double mean, final double sigma) {
            return (1.0 / (Math.sqrt(2 * Math.PI) * sigma)) * Math.exp( - ( ( (x - sigma) * (x - sigma))) / (2.0 * sigma * sigma));
        }

        synchronized public double nextGaussianSimple() {
            final double x = nextDouble() * 2 - 1;
            System.out.println("X: " + x);
            final double sigma = 1;
            final double mean = 0;
            return (1.0 / (Math.sqrt(2 * Math.PI) * sigma)) * Math.exp( -Math.pow(x - mean, 2) / (2.0 * Math.pow(sigma, 2)));
        }
    }

    public static void main(final String[] args) {
        final PlugInAlgorithmCreateSpheres701a p = new PlugInAlgorithmCreateSpheres701a();

        final long time = System.currentTimeMillis();
        System.out.println("Start");
        final DecimalFormat dec = new DecimalFormat("0.##");
        final RayleighRandom r = p.new RayleighRandom();
        final int[] rHit = new int[1000];
        for (int i = 0; i < 196608; i++) {
            final double ray = r.nextGaussianSimple();
            // System.out.println(ray);
            final long loc = Math.round( (ray * 100) + 500);
            if (ray < -3 || ray > 3) {
                System.out.println(loc + ", " + ray);
            }
            // System.out.println(r.nextRayleigh(.32, 0.0));
            rHit[(int) loc]++;
        }
        System.out.println("Start");
        for (int i = 0; i < rHit.length; i++) {
            System.out.println(rHit[i]);
        }

        System.out.println("Time: " + (System.currentTimeMillis() - time));
    }
}
