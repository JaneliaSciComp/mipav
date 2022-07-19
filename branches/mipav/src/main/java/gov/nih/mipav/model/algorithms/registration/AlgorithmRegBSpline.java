package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;

import java.util.StringTokenizer;


/**
 * This is a common base class which provides common methods and data members for all BSpline based registration.
 */
public abstract class AlgorithmRegBSpline extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Reference to an image to store the computed deformation. This may be a null reference to indicate that the
     * deformation is not to be computed. If this reference is not null, then the image must have the same dimensions as
     * the target image.
     */
    protected ModelImage m_kImageDeformation;

    /**
     * Reference to an image to store the registered source image. This image must have the same dimensions as the
     * registration target image.
     */
    protected ModelImage m_kImageResult;

    /**
     * Reference to the original input image to use as the registration source. This image does not have to have the
     * same dimensions as the image to be used as the registration target.
     */
    protected ModelImage m_kImageSource;

    /** Reference to the original input image to use as the registration target. */
    protected ModelImage m_kImageTarget;

    /** Options to use for a first pass of registration. This may *not* be a null reference. */
    protected Options m_kOptionsPass1;

    /**
     * Options to use for a second pass of registration. This may be null to indicate that only a single pass of
     * registration is to be performed.
     */
    protected Options m_kOptionsPass2;

    /**
     * Reference to the particular cost measure to use for this registration. The cost measure is the same for all
     * passes.
     */
    protected RegistrationMeasure m_kRegMeasure;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  kImageResult       ModelImage Reference to an image to store the registered source image. This image must
     *                            have the same dimensions as the registration target image.
     * @param  kImageSource       ModelImage Reference to the original input image to use as the registration source.
     *                            This image does not have to have the same dimensions as the image to be used as the
     *                            registration target.
     * @param  kImageTarget       ModelImage Reference to the original input image to use as the registration target.
     * @param  kImageDeformation  ModelImage Reference to an image to store the computed deformation. This may be a null
     *                            reference to indicate that the deformation is not to be computed. If this reference is
     *                            not null, then the image must have the same dimensions as the target image.
     * @param  kRegMeasure        RegistrationMeasure Reference to the particular cost measure to use for this
     *                            registration. The cost measure is the same for all passes.
     * @param  kOptionsPass1      Options Options to use for a first pass of registration. This may *not* be a null
     *                            reference.
     * @param  kOptionsPass2      Options Options to use for a second pass of registration. This may be null to indicate
     *                            that only a single pass of registration is to be performed.
     */
    protected AlgorithmRegBSpline(ModelImage kImageResult, ModelImage kImageSource, ModelImage kImageTarget,
                                  ModelImage kImageDeformation, RegistrationMeasure kRegMeasure, Options kOptionsPass1,
                                  Options kOptionsPass2) {

        super();

        // Save a copy of all the input parameters.
        m_kImageResult = kImageResult;
        m_kImageTarget = kImageTarget;
        m_kImageSource = kImageSource;
        m_kImageDeformation = kImageDeformation;
        m_kRegMeasure = kRegMeasure;
        m_kOptionsPass1 = kOptionsPass1;
        m_kOptionsPass2 = kOptionsPass2;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void disposeLocal() {
        m_kImageResult = null;
        m_kImageTarget = null;
        m_kImageSource = null;
        m_kImageDeformation = null;
        m_kOptionsPass1 = null;
        m_kOptionsPass2 = null;
        System.gc();
    }

    /**
     * DOCUMENT ME!
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Data structure which contains the parameters or options used to perform a BSpline based registration of two
     * images.
     */
    public static class Options {

        /**
         * Boolean flag set to true to indicate whether the registration is performed to a subsampled target image. This
         * may be used to speed up processing at some tradeoff for registration quality.
         */
        public boolean bSubsample;

        /**
         * One iteration is defined such that all control points are moved once by gradient descent. The registration
         * error before and after one iteration are compared and if the relative change is less than this limit, then
         * the registration terminates.
         */
        public float fConvergenceLimit;

        /** Size of step in sample units when locating the minimum of the gradient descent. */
        public float fGradientDescentMinimizeStepSize;

        /** Degree of the BSpline basis functions. Same for all dimensions. */
        public int iBSplineDegree;

        /** Number of control points to use for the open/uniform BSpline basis functions. Same for all dimensions. */
        public int iBSplineNumControlPoints;

        /**
         * Maximum number of steps to test when locating the minimum of the gradient descent. Combined with the
         * fGradientDescentMinimizeStepSize parameter, this determines the maximum distance from the current state to
         * search for the minimum of the gradient descent.
         */
        public int iGradientDescentMinimizeMaxSteps;

        /** Maximum number of iterations to perform if convergence limit condition is still not satisfied. */
        public int iMaxIterations;

        /**
         * DOCUMENT ME!
         *
         * @param   kOptions  String containing all of the options.
         * @param   kDelim    String to separate each parameter.
         *
         * @throws  TokenizerException  DOCUMENT ME!
         */
        public void setFromString(String kOptions, String kDelim) throws TokenizerException {
            StringTokenizer tok = new StringTokenizer(kOptions, kDelim);
            
            bSubsample = MipavUtil.getBoolean(tok);
            iBSplineDegree = MipavUtil.getInt(tok);
            iBSplineNumControlPoints = MipavUtil.getInt(tok);
            fGradientDescentMinimizeStepSize = MipavUtil.getFloat(tok);
            iGradientDescentMinimizeMaxSteps = MipavUtil.getInt(tok);
            fConvergenceLimit = MipavUtil.getFloat(tok);
            iMaxIterations = MipavUtil.getInt(tok);
        }

        /**
         * Convert the options to a delimeted text string.
         *
         * @param   kDelim  String String to separate each parameter.
         *
         * @return  String String which contains text representation of each option separated by the specified
         *          delimeter.
         */
        public String toString(String kDelim) {

            String kString = new String();

            kString += String.valueOf(bSubsample) + kDelim;
            kString += String.valueOf(iBSplineDegree) + kDelim;
            kString += String.valueOf(iBSplineNumControlPoints) + kDelim;
            kString += String.valueOf(fGradientDescentMinimizeStepSize) + kDelim;
            kString += String.valueOf(iGradientDescentMinimizeMaxSteps) + kDelim;
            kString += String.valueOf(fConvergenceLimit) + kDelim;
            kString += String.valueOf(iMaxIterations);

            return kString;
        }
    }
}
