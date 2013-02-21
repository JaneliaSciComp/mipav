package gov.nih.mipav.view;


import gov.nih.mipav.model.algorithms.levelset.*;
import gov.nih.mipav.model.structures.*;

import java.awt.event.*;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;


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
        int iXBound = imageA.getExtents().length > 0 ? imageA.getExtents()[0] : 1;
        int iYBound = imageA.getExtents().length > 1 ? imageA.getExtents()[1] : 1;
        int iZBound = imageA.getExtents().length > 2 ? imageA.getExtents()[2] : 1;
        int iQuantity = iXBound * iYBound * iZBound;
        float[] afImage = new float[iQuantity];

        for (int i = 0; i < iQuantity; i++) {
        	if ( imageA.isColorImage() )
        	{
        		afImage[i] = imageA.getFloat((i * 4) + 1);
        	}
        	else
        	{
        		afImage[i] = imageA.getFloat(i);        		
        	}
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
        VOIVector vois = imageA.getVOIs();
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
        for ( int i = 0; i < aiSeeds.length; i++ )
        {
        	System.err.println( "Seed " + i + " " + aiSeeds[i] );
        }
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
                            imageA.getMask().set(i);
                            //imageA.set((i * 4) + 1, fValue);
                            //imageA.set((i * 4) + 2, 0f);
                            //imageA.set((i * 4) + 3, 0f);
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
                    //float fValue = afImage[i];

                    if (kMarcher.isBoundary(i)) {
                        imageA.getMask().set(i);
                        //imageA.set((i * 4) + 1, 0f);
                        //imageA.set((i * 4) + 2, 0f);
                        //imageA.set((i * 4) + 3, fValue);
                    } else if (kMarcher.isFar(i)) {
                        //imageA.getMask().set(i);
                        //imageA.set((i * 4) + 1, fValue);
                        //imageA.set((i * 4) + 2, fValue);
                        //imageA.set((i * 4) + 3, fValue);
                    } else if (kMarcher.isZeroSpeed(i)) {
                        imageA.getMask().set(i);
                        //imageA.set((i * 4) + 1, fValue);
                        //imageA.set((i * 4) + 2, 0f);
                        //imageA.set((i * 4) + 3, 0f);
                    } else if (kMarcher.isTrial(i)) {
                        imageA.getMask().set(i);
                        //imageA.set((i * 4) + 1, 0f);
                        //imageA.set((i * 4) + 2, fValue);
                        //imageA.set((i * 4) + 3, 0f);
                    } else {

                        if (kMarcher.getTime(i) >= 0.0f) {
                            imageA.getMask().set(i);
                            //imageA.set((i * 4) + 1, fValue);
                            //imageA.set((i * 4) + 2, fValue);
                            //imageA.set((i * 4) + 3, 0f);
                        } else {
                            imageA.getMask().set(i);
                            //imageA.set((i * 4) + 1, fValue);
                            //imageA.set((i * 4) + 2, 0f);
                            //imageA.set((i * 4) + 3, fValue);
                        }
                    }
                }
            }
        }

        updateImages(true);
    }
}
