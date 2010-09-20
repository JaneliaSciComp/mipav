package gov.nih.mipav.view;


import gov.nih.mipav.model.algorithms.levelset.*;
import gov.nih.mipav.model.structures.*;

import java.awt.event.*;


/**
 * DOCUMENT ME!
 */
public class ViewJFrameFastMarching3 extends ViewJFrameImage implements KeyListener, Runnable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7827645314485923857L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean m_bIterate = true;

    /** DOCUMENT ME! */
    private boolean m_bNext = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------


    /**
     * Creates a new ViewJFrameFastMarching3 object.
     *
     * @param  _image  DOCUMENT ME!
     * @param  _LUT    DOCUMENT ME!
     */
    public ViewJFrameFastMarching3(ModelImage _image, ModelLUT _LUT) {

        // super(_image, null, null, null, null, null, ViewJFrameVolumeView.SURFACE, ViewJFrameVolumeView.NONE, null );
        super(_image, (ModelLUT)null);
        userInterface = ViewUserInterface.getReference();
        addKeyListener(this);
        new Thread(this).start();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * keyPressed.
     *
     * @param  e  KeyEvent
     */
    public void keyPressed(KeyEvent e) {
        int keyCode = e.getKeyCode();

        switch (keyCode) {

            case KeyEvent.VK_N:
                m_bNext = true;
                break;

            case KeyEvent.VK_S:
                m_bIterate = false;
                break;

            case KeyEvent.VK_G:
                m_bIterate = true;
                break;
        }
    }

    /**
     * keyReleased.
     *
     * @param  e  KeyEvent
     */
    public void keyReleased(KeyEvent e) { }

    /**
     * keyTyped.
     *
     * @param  e  KeyEvent
     */
    public void keyTyped(KeyEvent e) { }

    /**
     * DOCUMENT ME!
     */
    public void run() {

        // Load the image to be segmented.
        int iXBound = 128;
        int iYBound = 128;
        int iZBound = 128;
        int iQuantity = iXBound * iYBound * iZBound;
        float[] afImage = new float[iQuantity];

        for (int i = 0; i < iQuantity; i++) {
            afImage[i] = imageA.getFloat((i * 4) + 1);
        }

        // Create a segmenter for the image.
        float fXSpacing = 1.0f;
        float fYSpacing = 1.0f;
        float fZSpacing = 1.0f;
        boolean[] abMask = null;
        LseSegShapeDetection3 kSegmenter = new LseSegShapeDetection3(iXBound, iYBound, iZBound, fXSpacing, fYSpacing,
                                                                     fZSpacing, afImage, abMask);

        // Initialize the segmenter.
        float fTimeStep = 0.01f;
        int iDiffusionIterations = 10;
        float fGradientMagnitudeScale = 1.0f;
        float fSigmoidAlpha = -0.015f;
        float fSigmoidBeta = 0.0125f;
        float fSigmoidMin = 0.0f;
        float fSigmoidMax = 1.0f;
        kSegmenter.setPDEParameters(Float.MAX_VALUE, LsePdeFilter.UNIT, fTimeStep);
        kSegmenter.setDiffusionIterations(iDiffusionIterations);
        kSegmenter.setGradientMagnitudeScale(fGradientMagnitudeScale);
        kSegmenter.setSigmoidFilter(fSigmoidAlpha, fSigmoidBeta, fSigmoidMin, fSigmoidMax);

        System.err.println("FastMarching3 Start Coarse");

        // Grow a region consisting of a single seed.
        int iXSeed = 95, iYSeed = 67, iZSeed = 64;
        int[] aiSeeds = new int[1];
        aiSeeds[0] = iXSeed + (iXBound * (iYSeed + (iYBound * iZSeed)));
        kSegmenter.beginCoarse(aiSeeds);

        int iMaxCoarse = 1000;
        int i = 0;

        while (!m_bNext) {

            while (m_bIterate && !m_bNext && (i < iMaxCoarse)) {
                kSegmenter.iterateCoarse();
                DrawFastMarch(kSegmenter, afImage);
                i++;
            }

            if (i == iMaxCoarse) {
                m_bNext = true;
            }
        }

        m_bNext = false;
        kSegmenter.endCoarse();
        DrawFastMarch(kSegmenter, afImage);
        System.err.println("FastMarching3 Done Coarse");

        System.err.println("FastMarching3 Start Distance Transform");

        // Compute an annulus containing the coarse boundary.
        float fMaxDistance = 4.0f;
        kSegmenter.beginDistanceTransform(fMaxDistance);

        while (!m_bNext) {

            while (m_bIterate && !m_bNext) {

                if (kSegmenter.iterateDistanceTransform()) {
                    DrawFastMarch(kSegmenter, afImage);
                } else {
                    m_bNext = true;
                }
            }
        }

        m_bNext = false;
        kSegmenter.endDistanceTransform();
        DrawFastMarch(kSegmenter, afImage);
        System.err.println("FastMarching3 Done Distance Transform");

        System.err.println("FastMarching3 Start Evolution");

        // Evolve the coarse boundary.
        float fAdvectionWeight = 0.0f;
        float fPropagationWeight = 1.0f;
        float fCurvatureWeight = 0.05f;
        float fLaplacianWeight = 0.0f;
        float fEvolveTimeStep = 0.1f;
        kSegmenter.beginEvolution(fAdvectionWeight, fPropagationWeight, fCurvatureWeight, fLaplacianWeight,
                                  fEvolveTimeStep);

        int iMaxEvolution = 50;
        i = 0;

        while (!m_bNext) {

            while (m_bIterate && !m_bNext && (i < iMaxEvolution)) {
                kSegmenter.iterateEvolution();
                DrawEvolve(kSegmenter, afImage);
                i++;
            }

            if (i == iMaxEvolution) {
                m_bNext = true;
            }
        }

        m_bNext = false;
        kSegmenter.endEvolution();
        DrawEvolve(kSegmenter, afImage);
        System.err.println("FastMarching3 Done Evolution");
        System.err.println("FastMarching3 Done");
    }

    /**
     * DOCUMENT ME!
     *
     * @param  kSegmenter  DOCUMENT ME!
     * @param  afImage     DOCUMENT ME!
     */
    private void DrawEvolve(LseSegmenter kSegmenter, float[] afImage) {
        LsePdeFilter3 kFilter = (LsePdeFilter3) kSegmenter.getLevelSetEvolver();
        int iXBound = kFilter.getXBound();
        int iYBound = kFilter.getYBound();
        int iZBound = kFilter.getZBound();

        for (int iZ = 0, i = 0; iZ < iZBound; iZ++) {

            for (int iY = 0; iY < iYBound; iY++) {

                for (int iX = 0; iX < iXBound; iX++, i++) {
                    float fValue = afImage[i];
                    imageA.set((i * 4) + 1, fValue);
                    imageA.set((i * 4) + 2, fValue);
                    imageA.set((i * 4) + 3, fValue);

                    if (kFilter.getMask(iX, iY, iZ)) {
                        float fCenter = kFilter.getU(iX, iY, iZ);
                        float fDX = kFilter.getU(iX + 1, iY, iZ);
                        float fDY = kFilter.getU(iX, iY + 1, iZ);
                        float fDZ = kFilter.getU(iX, iY, iZ + 1);

                        if (((fDX * fCenter) < 0.0f) || ((fDY * fCenter) < 0.0f) || ((fDZ * fCenter) < 0.0f)) {
                            imageA.set((i * 4) + 1, fValue);
                            imageA.set((i * 4) + 2, 0f);
                            imageA.set((i * 4) + 3, 0f);
                        }
                    }
                }
            }
        }

        updateImages(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  kSegmenter  DOCUMENT ME!
     * @param  afImage     DOCUMENT ME!
     */
    private void DrawFastMarch(LseSegmenter kSegmenter, float[] afImage) {
        LseFastMarch3 kMarcher = (LseFastMarch3) kSegmenter.getFastMarcher();
        int iXBound = kMarcher.getXBound();
        int iYBound = kMarcher.getYBound();
        int iZBound = kMarcher.getZBound();

        for (int iZ = 0, i = 0; iZ < iZBound; iZ++) {

            for (int iY = 0; iY < iYBound; iY++) {

                for (int iX = 0; iX < iXBound; iX++, i++) {
                    float fValue = afImage[i];

                    if (kMarcher.isBoundary(i)) {
                        imageA.set((i * 4) + 1, 0f);
                        imageA.set((i * 4) + 2, 0f);
                        imageA.set((i * 4) + 3, fValue);
                    } else if (kMarcher.isFar(i)) {
                        imageA.set((i * 4) + 1, fValue);
                        imageA.set((i * 4) + 2, fValue);
                        imageA.set((i * 4) + 3, fValue);
                    } else if (kMarcher.isZeroSpeed(i)) {
                        imageA.set((i * 4) + 1, fValue);
                        imageA.set((i * 4) + 2, 0f);
                        imageA.set((i * 4) + 3, 0f);
                    } else if (kMarcher.isTrial(i)) {
                        imageA.set((i * 4) + 1, 0f);
                        imageA.set((i * 4) + 2, fValue);
                        imageA.set((i * 4) + 3, 0f);
                    } else {

                        if (kMarcher.getTime(i) >= 0.0f) {
                            imageA.set((i * 4) + 1, fValue);
                            imageA.set((i * 4) + 2, fValue);
                            imageA.set((i * 4) + 3, 0f);
                        } else {
                            imageA.set((i * 4) + 1, fValue);
                            imageA.set((i * 4) + 2, 0f);
                            imageA.set((i * 4) + 3, fValue);
                        }
                    }
                }
            }
        }

        updateImages(true);
    }
}
