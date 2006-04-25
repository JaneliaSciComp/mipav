package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * DOCUMENT ME!
 *
 * @version  0.1 November 3, 2005
 * @author   William Gandler This code is based on the material in the GLCM Texture Tutorial by Myrka Hall-Beyer at
 *           http://www.fp.ucalgary.ca/mhallbey/tutorial.html.
 *
 *           <p>The Haralick features are based on the frequency of occurrence of the pixel intensity values of 2 pixels
 *           an offset distance apart. The offset distance will almost always be 1. The frequency of occurrence is
 *           counted over a square window which is placed over each pixel interior to a band of width (window size -
 *           1)/2 at the edge of the image, since results are only calculated for pixels over which the entire square
 *           window can be placed. The values in this outer band have been left at zero.</p>
 *
 *           <p>The user selects 1 or more of the 5 direction possibilities and 1 or more of the 12 operator
 *           possibilities. The number of floating point Haralick texture images created equals the number of directions
 *           selected times the number of operators selected. The result image dimensions are the same as the source
 *           window dimensions.</p>
 *
 *           <p>The five direction possibilities are north-south, northeast-southwest, east-west, southeast-northwest,
 *           and spatially invariant. Spatially invariant is created by counting in the four directions and summing the
 *           counts.</p>
 *
 *           <p>The calculations start with the creation of a gray level co-occurrence matrix for each pixel position
 *           interior to the outer band for each direction. This matrix gives the probability for each pair of 2 pixels
 *           within the square window.</p>
 */
public class AlgorithmHaralickTexture extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** If true calculate angular second moment Sum over i,j of Probability(i,j) * Probability(i,j). */
    private boolean asm;

    /**
     * If true calculate contrast, also known as sum of squares variance Sum over i,j of Probability(i,j) * (i-j) *
     * (i-j).
     */
    private boolean contrast;

    /**
     * If true calculate glcm correlation Sum over i,j of Probability(i,j) * (i - glcm mean) * (j - glcm mean)/ glcm
     * variance.
     */
    private boolean correlation;

    /** DOCUMENT ME! */
    private ModelImage[] destImage = null;

    /** If true calculate dissimilarity Sum over i,j of Probability(i,j) * Absolute value(i-j). */
    private boolean dissimilarity;

    /** If true calculate energy Energy = square root(asm). */
    private boolean energy;

    /** If true calcuate entropy Sum over i,j of -Probability(i,j) * ln(Probability(i,j)). */
    private boolean entropy;

    /** If true east-west direction calculated. */
    private boolean ew;

    /**
     * If true calculate homogeneity, also called the Inverse Difference Moment of Order 2 Sum over i,j of
     * Probability(i,j)/(1 + (i-j)*(i-j)).
     */
    private boolean homogeneity;

    /**
     * If true spatially invariant direction calculated. This is created by counting in the 4 directions and summing the
     * counts.
     */
    private boolean invariantDir;

    /** If true calculate inverse difference moment of order 1 Sum over i,j of Probability(i,j)/(1 + abs(i-j)). */
    private boolean inverseOrder1;

    /** If true calculate maximum probability maximum probability is the largest probability value within the window. */
    private boolean maxProbability;

    /**
     * If true calculate gray level co-occurrence matrix mean Sum over i,j of i * Probability(i,j) = Sum over i,j of j *
     * Probability(i,j).
     */
    private boolean mean;

    /** If true northeast-southwest direction calculated. */
    private boolean nesw;

    /** If true north-south direction calculated. */
    private boolean ns;

    /** Distance between the 2 pixels in the pair - almost always 1. */
    private int offsetDistance = 1;

    /** If true southeast-northwest direction calculated. */
    private boolean senw;

    /** If true calculate glcm standard deviation glcm standard deviation = square root(glcm variance). */
    private boolean standardDeviation;

    /**
     * If true calculate glcm variance Sum over i,j of Probability(i,j) * (i - glcm mean) * (i - glcm mean) = Sum over
     * i,j of Probability(i,j) * (j - glcm mean) * (j - glcm mean).
     */
    private boolean variance;

    /** Size of square window used in calculating each glcm matrix - must be an odd number. */
    private int windowSize = 7;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmHaralickTexture object.
     *
     * @param              destImg            image model where result image is to stored
     * @param              srcImg             source image model
     * @param              windowSize         the size of the square window
     * @param              offsetDistance     distance between 2 pixels of pair
     * @param              ns                 true if north south offset direction calculated
     * @param              nesw               true if northeast-southwest offset direction calculated
     * @param              ew                 true if east west offset direction calculated
     * @param              senw               true if southeast-northwest offset direction calculated
     * @param              invariantDir       true if spatially invariant calculated
     * @param              contrast           true if contrast calculated
     * @param              dissimilarity      true if dissimilarity calculated
     * @param              homogeneity        true if homogeneity calculated
     * @param              inverseOrder1      true if inverse difference moment of order 1 calculated
     * @param              asm                true if angular second moment calculated
     * @param              energy             true if energy calculated
     * @param              maxProbability     true if maximum probability calculated
     * @param              entropy            true if entropy calculated
     * @param              mean               true if gray level coordinate matrix mean calculated
     * @param              variance           DOCUMENT ME!
     * @param              standardDeviation  DOCUMENT ME!
     * @param              correlation        DOCUMENT ME!
     *
     * @variance           true if gray level coordinate matrix variance calculated
     * @standardDeviation  true if gray level coordinate matrix standard deviation calculated
     * @coorelation        true if gray level coordinate matrix correlation calculated
     */
    public AlgorithmHaralickTexture(ModelImage[] destImg, ModelImage srcImg, int windowSize, int offsetDistance,
                                    boolean ns, boolean nesw, boolean ew, boolean senw, boolean invariantDir,
                                    boolean contrast, boolean dissimilarity, boolean homogeneity, boolean inverseOrder1,
                                    boolean asm, boolean energy, boolean maxProbability, boolean entropy, boolean mean,
                                    boolean variance, boolean standardDeviation, boolean correlation) {
        super(null, srcImg);
        destImage = destImg;
        this.windowSize = windowSize;
        this.offsetDistance = offsetDistance;
        this.ns = ns;
        this.nesw = nesw;
        this.ew = ew;
        this.senw = senw;
        this.invariantDir = invariantDir;
        this.contrast = contrast;
        this.dissimilarity = dissimilarity;
        this.homogeneity = homogeneity;
        this.inverseOrder1 = inverseOrder1;
        this.asm = asm;
        this.energy = energy;
        this.maxProbability = maxProbability;
        this.entropy = entropy;
        this.mean = mean;
        this.variance = variance;
        this.standardDeviation = standardDeviation;
        this.correlation = correlation;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }


    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        constructLog();

        calculateHaralick();
    }

    /**
     * DOCUMENT ME!
     */
    private void calculateHaralick() {
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int sourceMax;
        boolean doneNS = false;
        boolean doneNESW = false;
        boolean doneEW = false;
        boolean doneSENW = false;
        boolean doneInvariant = false;
        int iDir;
        srcImage.calcMinMax();
        sourceMax = (int) Math.round(srcImage.getMax());

        int numDirections = 0;
        int numOperators = 0;
        short[] sourceBuffer = new short[sliceSize];
        int[][] nsBuffer = null;
        int[][] neswBuffer = null;
        int[][] ewBuffer = null;
        int[][] senwBuffer = null;
        int[][] invariantDirBuffer = null;

        // gray level co-occurrence matrix
        float[][] glcm = null;
        int matrixSize;
        int halfWin = (windowSize - 1) / 2;
        int xStart = halfWin;
        int xEnd = xDim - 1 - halfWin;
        int yStart = halfWin;
        int yEnd = yDim - 1 - halfWin;
        int x, y;
        int i, j;
        int index;
        int resultNumber;
        float matrixSum;
        float[][] resultBuffer = null;
        int currentResult;
        int pos;
        float glcmMean = 0.0f;
        float glcmVariance = 0.0f;
        float glcmASM = 0.0f;
        boolean skip;

        buildProgressBar(srcImage.getImageName(), "Running Haralick textures...", 0, 100);
        initProgressBar();

        try {
            srcImage.exportData(0, sliceSize, sourceBuffer);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmHaralickTexture: IOException on srcImage.exportData(0,sliceSize,sourceBuffer)");
            progressBar.dispose();
            setCompleted(false);

            return;
        }

        if (ns) {
            numDirections++;
        }

        if (nesw) {
            numDirections++;
        }

        if (ew) {
            numDirections++;
        }

        if (senw) {
            numDirections++;
        }

        if (invariantDir) {
            numDirections++;
        }

        if (contrast) {
            numOperators++;
        }

        if (dissimilarity) {
            numOperators++;
        }

        if (homogeneity) {
            numOperators++;
        }

        if (inverseOrder1) {
            numOperators++;
        }

        if (asm) {
            numOperators++;
        }

        if (energy) {
            numOperators++;
        }

        if (maxProbability) {
            numOperators++;
        }

        if (entropy) {
            numOperators++;
        }

        if (mean) {
            numOperators++;
        }

        if (variance) {
            numOperators++;
        }

        if (standardDeviation) {
            numOperators++;
        }

        if (correlation) {
            numOperators++;
        }

        resultNumber = numDirections * numOperators;

        matrixSize = sourceMax + 1;

        if (ns || invariantDir) {
            nsBuffer = new int[matrixSize][matrixSize];
        }

        if (nesw || invariantDir) {
            neswBuffer = new int[matrixSize][matrixSize];
        }

        if (ew || invariantDir) {
            ewBuffer = new int[matrixSize][matrixSize];
        }

        if (senw || invariantDir) {
            senwBuffer = new int[matrixSize][matrixSize];
        }

        if (invariantDir) {
            invariantDirBuffer = new int[matrixSize][matrixSize];
        }

        glcm = new float[matrixSize][matrixSize];
        resultBuffer = new float[resultNumber][sliceSize];

        for (y = yStart; (y <= yEnd) && !threadStopped; y++) {
            progressBar.updateValue((int) ((y - yStart) * (100.0f / (yEnd - yStart))), activeImage);

            for (x = xStart; x <= xEnd; x++) {
                pos = x + (y * xDim);
                doneNS = false;
                doneNESW = false;
                doneEW = false;
                doneSENW = false;
                doneInvariant = false;
                currentResult = 0;

                for (iDir = 0; iDir < numDirections; iDir++) {
                    skip = false;

                    if ((ns || invariantDir) && (!doneNS)) {
                        doneNS = true;

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {
                                nsBuffer[i][j] = 0;
                            }
                        }

                        for (j = y - halfWin; j <= (y + halfWin - offsetDistance); j++) {

                            for (i = x - halfWin; i <= (x + halfWin); i++) {
                                index = i + (j * xDim);
                                nsBuffer[sourceBuffer[index]][sourceBuffer[index + xDim]]++;
                                nsBuffer[sourceBuffer[index + xDim]][sourceBuffer[index]]++;
                            }
                        }

                        if (ns) {
                            matrixSum = 2.0f * (windowSize - offsetDistance) * windowSize;

                            for (i = 0; i < matrixSize; i++) {

                                for (j = 0; j < matrixSize; j++) {
                                    glcm[i][j] = nsBuffer[i][j] / matrixSum;
                                }
                            }

                            skip = true;
                        } // if (ns)
                    }

                    if ((nesw || invariantDir) && (!doneNESW) && (!skip)) {
                        doneNESW = true;

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {
                                neswBuffer[i][j] = 0;
                            }
                        }

                        for (j = y - halfWin; j <= (y + halfWin - offsetDistance); j++) {

                            for (i = x - halfWin + offsetDistance; i <= (x + halfWin); i++) {
                                index = i + (j * xDim);
                                neswBuffer[sourceBuffer[index]][sourceBuffer[index + xDim - 1]]++;
                                neswBuffer[sourceBuffer[index + xDim - 1]][sourceBuffer[index]]++;
                            }
                        }

                        if (nesw) {
                            matrixSum = 2.0f * (windowSize - offsetDistance) * (windowSize - offsetDistance);

                            for (i = 0; i < matrixSize; i++) {

                                for (j = 0; j < matrixSize; j++) {
                                    glcm[i][j] = neswBuffer[i][j] / matrixSum;
                                }
                            }

                            skip = true;
                        } // if (nesw)
                    }

                    if ((ew || invariantDir) && (!doneEW) && (!skip)) {
                        doneEW = true;

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {
                                ewBuffer[i][j] = 0;
                            }
                        }

                        for (j = y - halfWin; j <= (y + halfWin); j++) {

                            for (i = x - halfWin; i <= (x + halfWin - offsetDistance); i++) {
                                index = i + (j * xDim);
                                ewBuffer[sourceBuffer[index]][sourceBuffer[index + 1]]++;
                                ewBuffer[sourceBuffer[index + 1]][sourceBuffer[index]]++;
                            }
                        }

                        if (ew) {
                            matrixSum = 2.0f * (windowSize - offsetDistance) * windowSize;

                            for (i = 0; i < matrixSize; i++) {

                                for (j = 0; j < matrixSize; j++) {
                                    glcm[i][j] = ewBuffer[i][j] / matrixSum;
                                }
                            }

                            skip = true;
                        } // if (ew)
                    }

                    if ((senw || invariantDir) && (!doneSENW) && (!skip)) {
                        doneSENW = true;

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {
                                senwBuffer[i][j] = 0;
                            }
                        }

                        for (j = y - halfWin; j <= (y + halfWin - offsetDistance); j++) {

                            for (i = x - halfWin; i <= (x + halfWin - offsetDistance); i++) {
                                index = i + (j * xDim);
                                senwBuffer[sourceBuffer[index]][sourceBuffer[index + xDim + 1]]++;
                                senwBuffer[sourceBuffer[index + xDim + 1]][sourceBuffer[index]]++;
                            }
                        }

                        if (senw) {
                            matrixSum = 2.0f * (windowSize - offsetDistance) * (windowSize - offsetDistance);

                            for (i = 0; i < matrixSize; i++) {

                                for (j = 0; j < matrixSize; j++) {
                                    glcm[i][j] = senwBuffer[i][j] / matrixSum;
                                }
                            }

                            skip = true;
                        } // if (senw)
                    }

                    if (invariantDir && (!doneInvariant) && (!skip)) {
                        doneInvariant = true;

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {
                                invariantDirBuffer[i][j] = nsBuffer[i][j] + neswBuffer[i][j] + ewBuffer[i][j] +
                                                           senwBuffer[i][j];
                            }
                        }

                        matrixSum = (4.0f * (windowSize - offsetDistance) * (windowSize - offsetDistance)) +
                                    (4.0f * (windowSize - offsetDistance) * windowSize);

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {
                                glcm[i][j] = invariantDirBuffer[i][j] / matrixSum;
                            }
                        }
                    } // else if (invariantDir && (!doneInvariant)

                    if (contrast) {

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {
                                resultBuffer[currentResult][pos] += (i - j) * (i - j) * glcm[i][j];
                            }
                        }

                        currentResult++;
                    } // if (contrast)

                    if (dissimilarity) {

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {
                                resultBuffer[currentResult][pos] += Math.abs(i - j) * glcm[i][j];
                            }
                        }

                        currentResult++;
                    } // if (dissimilarity)

                    if (homogeneity) {

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {
                                resultBuffer[currentResult][pos] += glcm[i][j] / (1.0f + ((i - j) * (i - j)));
                            }
                        }

                        currentResult++;
                    } // if (homogeneity)

                    if (inverseOrder1) {

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {
                                resultBuffer[currentResult][pos] += glcm[i][j] / (1.0f + Math.abs(i - j));
                            }
                        }

                        currentResult++;
                    } // if (inverseOrder1)

                    if (asm || energy) {
                        glcmASM = 0.0f;

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {
                                glcmASM += glcm[i][j] * glcm[i][j];
                            }
                        }

                        if (!energy) {
                            resultBuffer[currentResult][pos] = glcmASM;
                            currentResult++;
                        } else if (!asm) {
                            resultBuffer[currentResult][pos] = (float) Math.sqrt(glcmASM);
                            currentResult++;
                        } else {
                            resultBuffer[currentResult][pos] = glcmASM;
                            currentResult++;
                            resultBuffer[currentResult][pos] = (float) Math.sqrt(glcmASM);
                            currentResult++;
                        }
                    } // if (asm || energy)

                    if (maxProbability) {

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {

                                if (glcm[i][j] > resultBuffer[currentResult][pos]) {
                                    resultBuffer[currentResult][pos] = glcm[i][j];
                                }
                            }
                        }

                        currentResult++;
                    } // if (maxProbability)

                    if (entropy) {

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {

                                if (glcm[i][j] > 0.0f) {
                                    resultBuffer[currentResult][pos] -= glcm[i][j] * Math.log(glcm[i][j]);
                                }
                            }
                        }

                        currentResult++;
                    } // if (entropy)

                    if (mean || variance || standardDeviation || correlation) {
                        glcmMean = 0.0f;

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {
                                glcmMean += i * glcm[i][j];
                            }
                        }

                        if (mean) {
                            resultBuffer[currentResult][pos] = glcmMean;
                            currentResult++;
                        }
                    } // if (mean || variance || standardDeviation || correlation)

                    if (variance || standardDeviation || correlation) {
                        glcmVariance = 0.0f;

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {
                                glcmVariance += glcm[i][j] * (i - glcmMean) * (i - glcmMean);
                            }
                        }

                        if ((!standardDeviation) && variance) {
                            resultBuffer[currentResult][pos] = glcmVariance;
                            currentResult++;
                        } else if ((!variance) && standardDeviation) {
                            resultBuffer[currentResult][pos] = (float) Math.sqrt(glcmVariance);
                            currentResult++;
                        } else if (variance && standardDeviation) {
                            resultBuffer[currentResult][pos] = glcmVariance;
                            currentResult++;
                            resultBuffer[currentResult][pos] = (float) Math.sqrt(glcmVariance);
                            currentResult++;
                        }
                    } // if (variance || standardDeviation)

                    if (correlation) {

                        for (i = 0; i < matrixSize; i++) {

                            for (j = 0; j < matrixSize; j++) {
                                resultBuffer[currentResult][pos] += glcm[i][j] * (i - glcmMean) * (j - glcmMean) /
                                                                        glcmVariance;
                            }
                        }

                        currentResult++;
                    } // if (correlation)
                } // for (iDir = 0; iDir < numDirections; iDir++)
            } // for (x = xStart; x <= xEnd; x++)
        } // for (y = yStart; y <= yEnd && !threadStopped; y++)

        if (threadStopped) {
            finalize();

            return;
        }

        for (i = 0; i < resultNumber; i++) {

            try {
                destImage[i].importData(0, resultBuffer[i], true);
            } catch (IOException error) {
                MipavUtil.displayError("AlgorithmHaralickTexture: IOException on destImage[" + i +
                                       "].importData(0,resultBuffer[" + i + "],true)");
                progressBar.dispose();
                setCompleted(false);

                return;
            }
        } // for (i = 0; i < resultNumber; i++)

        progressBar.dispose();
        setCompleted(true);

        return;
    }


    /**
     * Constructs a string of the contruction parameters and out puts the string to the messsage frame if the history
     * logging procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("HaralickTexture(" + String.valueOf(windowSize) + ", " +
                                   String.valueOf(offsetDistance) + ", " + String.valueOf(ns) + ", " +
                                   String.valueOf(nesw) + ", " + String.valueOf(ew) + ", " + String.valueOf(senw) +
                                   ", " + String.valueOf(invariantDir) + ", " + String.valueOf(contrast) + "," +
                                   String.valueOf(dissimilarity) + "," + String.valueOf(homogeneity) + "," +
                                   String.valueOf(inverseOrder1) + "," + String.valueOf(asm) + "," +
                                   String.valueOf(energy) + "," + String.valueOf(maxProbability) + "," +
                                   String.valueOf(entropy) + "," + String.valueOf(mean) + "," +
                                   String.valueOf(variance) + "," + String.valueOf(standardDeviation) + "," +
                                   String.valueOf(correlation) + ")" + "\n");
    }

}
