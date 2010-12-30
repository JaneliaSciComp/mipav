package gov.nih.mipav.model.algorithms.levelset;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * DOCUMENT ME!
 */
public class AlgorithmFastMarching3 extends AlgorithmBase {

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7827645314485923857L;

    /** DOCUMENT ME! */
    private boolean m_bIterate = true;

    /** DOCUMENT ME! */
    private boolean m_bNext = false;

    /** The number of iterations to use in the nonlinear diffusion
     * (curvature flow filter) applied to the input image. */
    private int m_iDiffusionIterations = 10;
    /** The scale to use in computing the blurred gradient magnitude of the curvature flow image. */
    private float m_fGradientMagnitudeScale = 1.0f;
    /**  The parameters for the sigmoid function through which the
     * blurred gradient magnitude image is processed.  The variance of the function. */
    private float m_fSigmoidAlpha = -0.015f;
    /**  The parameters for the sigmoid function through which the
     * blurred gradient magnitude image is processed.  The center of the function. */
    private float m_fSigmoidBeta = 0.0125f;
    /**  The parameters for the sigmoid function through which the
     * blurred gradient magnitude image is processed.  The minimum of the function. */
    private float m_fSigmoidMin = 0.0f;
    /**  The parameters for the sigmoid function through which the
     * blurred gradient magnitude image is processed.  The maximum of the function. */
    private float m_fSigmoidMax = 1.0f;

    /** Number of coarse iterations */
    private int m_iMaxCoarse = 1000;
    /** The maximum distance to allow when computing the signed distance transform. */
    private float m_fMaxDistance = 4.0f;
    /** The advection coefficient 'a' in the PDE that controls the evolution. */
    private float m_fAdvectionWeight = 0.0f;
    /** The propagation coefficient 'b' in the PDE that controls the evolution. */
    private float m_fPropagationWeight = 1.0f;
    /** The curvature coefficient 'c' in the PDE that controls the evolution.*/
    private float m_fCurvatureWeight = 0.05f;
    /** The Laplacian coefficient 'd' in the PDE that controls the evolution. */
    private float m_fLaplacianWeight = 0.0f;
    /** Number of evolution iterations */
    private int m_iMaxEvolution = 50;

    //~ Constructors ---------------------------------------------------------------------------------------------------


    /**
     * Creates a new ViewJFrameFastMarching3 object.
     *
     * @param  _image  DOCUMENT ME!
     * @param  _LUT    DOCUMENT ME!
     */
    public AlgorithmFastMarching3(ModelImage _image) {
        super(null, _image);
    }

    /**
     * Creates a new AlgorithmFastMarching3 object.
     *
     * @param  _image    reference to the source image
     * @param iIters The number of iterations to use in the nonlinear diffusion (curvature flow filter) applied to the input image.
     * @param fGMScale The scale to use in computing the blurred gradient magnitude of the curvature flow image.
     * @param fSAlpha The parameters for the sigmoid function through which the
     * blurred gradient magnitude image is processed.  The variance of the function.
     * @param fSBeta The parameters for the sigmoid function through which the
     * blurred gradient magnitude image is processed.  The center of the function.
     * @param fSMin The parameters for the sigmoid function through which the
     * blurred gradient magnitude image is processed.  The minimum of the function.
     * @param fSMax The parameters for the sigmoid function through which the
     * blurred gradient magnitude image is processed.  The maximum of the function.
     * @param iCoarseMax Number of coarse iterations
     * @param fMaxDistance The maximum distance to allow when computing the
     *     signed distance transform.
     * * @param fAdvectionWeight The advection coefficient 'a' in the PDE that
     *     controls the evolution.
     * @param fPropagationWeight The propagation coefficient 'b' in the PDE
     *     that controls the evolution.
     * @param fCurvatureWeight The curvature coefficient 'c' in the PDE that
     *     controls the evolution.
     * @param fLaplacianWeight The Laplacian coefficient 'd' in the PDE that
     *     controls the evolution.
     * @param iEvolveMax Number of evolution iterations
     */
    public AlgorithmFastMarching3(ModelImage _image, int iIters, float fGMScale, 
    		float fSAlpha, float fSBeta, float fSMin, float fSMax, 
    		int iCoarseMax, float fMaxDistance, 
    		float fAdvectionWeight,	float fPropagationWeight, float fCurvatureWeight, float fLaplacianWeight, 
    		int iEvolveMax) {
        super(null, _image);
        m_iDiffusionIterations = iIters;
        m_fGradientMagnitudeScale = fGMScale;
        m_fSigmoidAlpha = fSAlpha;
        m_fSigmoidBeta = fSBeta;
        m_fSigmoidMin = fSMin;
        m_fSigmoidMax = fSMax;

        m_iMaxCoarse = iCoarseMax;
        m_fMaxDistance = fMaxDistance;
        m_fAdvectionWeight = fAdvectionWeight;
        m_fPropagationWeight = fPropagationWeight;
        m_fCurvatureWeight = fCurvatureWeight;
        m_fLaplacianWeight = fLaplacianWeight;
        m_iMaxEvolution = iEvolveMax;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    @Override
	public void runAlgorithm() {

        // Load the image to be segmented.
        int iXBound = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
        int iYBound = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;
        int iZBound = srcImage.getExtents().length > 2 ? srcImage.getExtents()[2] : 1;
        int iQuantity = iXBound * iYBound * iZBound;
        float[] afImage = new float[iQuantity];

        for (int i = 0; i < iQuantity; i++) {
        	if ( srcImage.isColorImage() )
        	{
        		afImage[i] = srcImage.getFloat((i * 4) + 1);
        	}
        	else
        	{
        		afImage[i] = srcImage.getFloat(i);        		
        	}
        }

        // Create a segmenter for the image.
        float fXSpacing = srcImage.getExtents().length > 0 ? srcImage.getResolutions(0)[0] : 1;
        float fYSpacing = srcImage.getExtents().length > 1 ? srcImage.getResolutions(0)[1] : 1;
        float fZSpacing = srcImage.getExtents().length > 2 ? srcImage.getResolutions(0)[2] : 1;
        boolean[] abMask = null;
        LseSegShapeDetection3 kSegmenter = new LseSegShapeDetection3(iXBound, iYBound, iZBound, fXSpacing, fYSpacing,
                                                                     fZSpacing, afImage, abMask);

        // Initialize the segmenter.
        float fTimeStep = 0.01f;
        kSegmenter.setPDEParameters(Float.MAX_VALUE, LsePdeFilter.UNIT, fTimeStep);
        kSegmenter.setDiffusionIterations(m_iDiffusionIterations);
        kSegmenter.setGradientMagnitudeScale(m_fGradientMagnitudeScale);
        kSegmenter.setSigmoidFilter(m_fSigmoidAlpha, m_fSigmoidBeta, m_fSigmoidMin, m_fSigmoidMax);

        fireProgressStateChanged(srcImage.getImageName(), "FastMarching3 Start Coarse");

        // Grow a region consisting of a single seed.
        VOIVector vois = srcImage.getVOIs();
        Vector<VOIPoint> kPointVOIs = new Vector<VOIPoint>();
        for ( int i = 0; i < vois.size(); i++ )
        {
            for ( int j = 0; j < vois.elementAt(i).getCurves().size(); j++ )
            {
            	if ( vois.elementAt(i).getCurves().elementAt(j).getType() == VOI.POINT ) 
            	{
            		kPointVOIs.add( (VOIPoint)vois.elementAt(i).getCurves().elementAt(j) );
            	}
            }
        }
        int[] aiSeeds = new int[kPointVOIs.size()];
        for ( int i = 0; i < aiSeeds.length; i++ )
        {
        	Vector3f kPos = kPointVOIs.elementAt(i).elementAt(0);
        	aiSeeds[i] = (int)(kPos.X + (iXBound * (kPos.Y + (iYBound * kPos.Z))));
        }
        if ( aiSeeds.length == 0 )
        {
        	aiSeeds = new int[1];
            int iXSeed = iXBound/2, iYSeed = iYBound/2, iZSeed = iZBound/2;
        	aiSeeds[0] = iXSeed + (iXBound * (iYSeed + (iYBound * iZSeed)));
        }
        kSegmenter.beginCoarse(aiSeeds);

        int i = 0;
        float percent = 100f/(float)(m_iMaxCoarse + m_iMaxEvolution);
        while (!m_bNext) {

            while (m_bIterate && !m_bNext && (i < m_iMaxCoarse)) {
                kSegmenter.iterateCoarse();
                DrawFastMarch(kSegmenter);
                i++;
                fireProgressStateChanged((int)(i*percent));
            }

            if (i == m_iMaxCoarse) {
                m_bNext = true;
            }
        }

        m_bNext = false;
        kSegmenter.endCoarse();
        DrawFastMarch(kSegmenter);

        fireProgressStateChanged(srcImage.getImageName(), "FastMarching3 Start Distance Transform");

        // Compute an annulus containing the coarse boundary.
        kSegmenter.beginDistanceTransform(m_fMaxDistance);

        while (!m_bNext) {

            while (m_bIterate && !m_bNext) {

                if (kSegmenter.iterateDistanceTransform()) {
                    DrawFastMarch(kSegmenter);
                } else {
                    m_bNext = true;
                }
            }
        }

        m_bNext = false;
        kSegmenter.endDistanceTransform();
        DrawFastMarch(kSegmenter);

        fireProgressStateChanged(srcImage.getImageName(), "FastMarching3 Start Evolution");

        float fEvolveTimeStep = 0.1f;
        // Evolve the coarse boundary.
        kSegmenter.beginEvolution(m_fAdvectionWeight, m_fPropagationWeight, m_fCurvatureWeight, m_fLaplacianWeight,
                                  fEvolveTimeStep);

        i = 0;

        while (!m_bNext) {

            while (m_bIterate && !m_bNext && (i < m_iMaxEvolution)) {
                kSegmenter.iterateEvolution();
                DrawEvolve(kSegmenter);
                i++;
                fireProgressStateChanged((int)((m_iMaxCoarse + i)*percent));
            }

            if (i == m_iMaxEvolution) {
                m_bNext = true;
            }
        }

        m_bNext = false;
        kSegmenter.endEvolution();
        DrawEvolve(kSegmenter);
        setCompleted(true);
        fireProgressStateChanged(100);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  kSegmenter  DOCUMENT ME!
     * @param  afImage     DOCUMENT ME!
     */
    private void DrawEvolve(LseSegmenter kSegmenter) {
        LsePdeFilter3 kFilter = (LsePdeFilter3) kSegmenter.getLevelSetEvolver();
        int iXBound = kFilter.getXBound();
        int iYBound = kFilter.getYBound();
        int iZBound = kFilter.getZBound();

        for (int iZ = 0, i = 0; iZ < iZBound; iZ++) {

            for (int iY = 0; iY < iYBound; iY++) {

                for (int iX = 0; iX < iXBound; iX++, i++) {
                    //float fValue = afImage[i];
                    //imageA.set((i * 4) + 1, fValue);
                    //imageA.set((i * 4) + 2, fValue);
                    //imageA.set((i * 4) + 3, fValue);

                    if (kFilter.getMask(iX, iY, iZ)) {
                        float fCenter = kFilter.getU(iX, iY, iZ);
                        float fDX = kFilter.getU(iX + 1, iY, iZ);
                        float fDY = kFilter.getU(iX, iY + 1, iZ);
                        float fDZ = kFilter.getU(iX, iY, iZ + 1);

                        if (((fDX * fCenter) < 0.0f) || ((fDY * fCenter) < 0.0f) || ((fDZ * fCenter) < 0.0f)) {
                        	srcImage.getMask().set(i);
                            //imageA.set((i * 4) + 1, fValue);
                            //imageA.set((i * 4) + 2, 0f);
                            //imageA.set((i * 4) + 3, 0f);
                        }
                    }
                }
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  kSegmenter  DOCUMENT ME!
     * @param  afImage     DOCUMENT ME!
     */
    private void DrawFastMarch(LseSegmenter kSegmenter) {
        LseFastMarch3 kMarcher = (LseFastMarch3) kSegmenter.getFastMarcher();
        int iXBound = kMarcher.getXBound();
        int iYBound = kMarcher.getYBound();
        int iZBound = kMarcher.getZBound();

        for (int iZ = 0, i = 0; iZ < iZBound; iZ++) {

            for (int iY = 0; iY < iYBound; iY++) {

                for (int iX = 0; iX < iXBound; iX++, i++) {
                    //float fValue = afImage[i];

                    if (kMarcher.isBoundary(i)) {
                    	srcImage.getMask().set(i);
                        //imageA.set((i * 4) + 1, 0f);
                        //imageA.set((i * 4) + 2, 0f);
                        //imageA.set((i * 4) + 3, fValue);
                    } else if (kMarcher.isFar(i)) {
                        //imageA.getMask().set(i);
                        //imageA.set((i * 4) + 1, fValue);
                        //imageA.set((i * 4) + 2, fValue);
                        //imageA.set((i * 4) + 3, fValue);
                    } else if (kMarcher.isZeroSpeed(i)) {
                    	srcImage.getMask().set(i);
                        //imageA.set((i * 4) + 1, fValue);
                        //imageA.set((i * 4) + 2, 0f);
                        //imageA.set((i * 4) + 3, 0f);
                    } else if (kMarcher.isTrial(i)) {
                    	srcImage.getMask().set(i);
                        //imageA.set((i * 4) + 1, 0f);
                        //imageA.set((i * 4) + 2, fValue);
                        //imageA.set((i * 4) + 3, 0f);
                    } else {

                        if (kMarcher.getTime(i) >= 0.0f) {
                        	srcImage.getMask().set(i);
                            //imageA.set((i * 4) + 1, fValue);
                            //imageA.set((i * 4) + 2, fValue);
                            //imageA.set((i * 4) + 3, 0f);
                        } else {
                        	srcImage.getMask().set(i);
                            //imageA.set((i * 4) + 1, fValue);
                            //imageA.set((i * 4) + 2, 0f);
                            //imageA.set((i * 4) + 3, fValue);
                        }
                    }
                }
            }
        }
    }
}
