package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.actions.ActionMaskToVOI;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import java.util.*;

import javax.swing.*;



public class JDialogLoadProstateMask extends JDialogBase implements AlgorithmInterface
{

    /** DOCUMENT ME! */
    private ModelImage image; // source image
    /** The main user interface. */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private AlgorithmThresholdDual thresholdAlgo;

    private AlgorithmMorphology2D particleAlgo2D;

    private AlgorithmMorphology2D idObjectsAlgo2D;

    private AlgorithmBSmooth smoothAlgo;

    /** Same data-type, binary, or unsigned byte mask. */
    private int outputType = 0;

    /** DOCUMENT ME! */
    private float min, max;


    /** DOCUMENT ME! */
    private ViewVOIVector VOIs;
    /** DOCUMENT ME! */
    private int groupNum;
    /** DOCUMENT ME! */
    private Color voiColor;
    /** DOCUMENT ME! */
    private int elementNum;
    private int defaultPts;
    /** DOCUMENT ME! */
    private boolean trim = true;

    private double originalBuffer[];


    public JDialogLoadProstateMask(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        UI = ViewUserInterface.getReference();
        image = im;
        init();
    }

    public void init() {
        String fileName;
        String directory;
        JFileChooser chooser = new JFileChooser();

        chooser.setDialogTitle("Open Prostate Mask");

        if (UI.getDefaultDirectory() != null) {
            File file = new File(UI.getDefaultDirectory());

            if (file != null) {
                chooser.setCurrentDirectory(file);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        int returnValue = chooser.showOpenDialog(UI.getMainFrame());

        if (returnValue == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

            readMaskfile(fileName, directory);

        } else {
            return;
        }      
    }

    public void readMaskfile(String fileName, String directory) {
        try {
            FileReader fileReader = new FileReader(directory
                    + File.separatorChar + fileName);
            BufferedReader inputStream = new BufferedReader(fileReader);

            String line = null;
            float value;
            Vector mask = new Vector();
            while ( (line = inputStream.readLine()) != null ) {

                value = Float.valueOf(line);

                mask.add(value);
                // System.err.println("value = " + value);
            }

            int xDim = image.getExtents()[0];
            int yDim = image.getExtents()[1];
            int sliceSize = xDim * yDim;
            double[] sourceBuffer = new double[sliceSize];
            originalBuffer = new double[sliceSize];

            image.exportData(0, sliceSize, sourceBuffer);
            image.exportData(0, sliceSize, originalBuffer);

            for ( int i = 0; i < sourceBuffer.length; i++ ) {
                value = (Float)mask.get(i);
                if ( value == 1 ) {
                    // sourceBuffer[i] = 1.0;
                } else {
                    sourceBuffer[i] = 0.0;
                }
            }

            ModelImage resultImage = (ModelImage)image.clone();
            resultImage.importData(0, sourceBuffer, true);
            resultImage.calcMinMax();
            resultImage.notifyImageDisplayListeners(null, true);

            new ViewJFrameImage(resultImage);

            image.importData(0, sourceBuffer, true);
            image.calcMinMax();
            image.notifyImageDisplayListeners(null, true);



            morphologyPostProcessing();

        } catch (FileNotFoundException e) {
            System.err.println("ERROR: Can't find file " + fileName);
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    public void morphologyPostProcessing() {
        threshold();
        particleSystem();
        IDobjects();
        maskToVOI();
        smoothVOI();
    }

    public void particleSystem() {
        try {
            // No need to make new image space because the user has choosen to replace the source image Make the
            // algorithm class particleAlgo2D = new AlgorithmMorphology2D( image, kernel, kernelSize,
            // AlgorithmMorphology2D.PARTICLE_ANALYSIS_NEW,       itersD, 0, 0, 0, regionFlag );
            particleAlgo2D = new AlgorithmMorphology2D(image, 0, 1.0f, 0, 1.0f,
                    AlgorithmMorphology2D.PARTICLE_ANALYSIS_NEW, 1,
                    1, 0, 0, true, false);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            particleAlgo2D.addListener(this);


            // Hide the dialog since the algorithm is about to run.
            setVisible(false);

            particleAlgo2D.run();

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog particle analysis new: unable to allocate enough memory");

            return;
        }
    }

    public void threshold() {
        float[] thresholds = new float[2];

        float thres1 = 0f;      // lowerThres;
        float thres2 = 0.999f;  // upperThres;

        thresholds[0] = thres1;
        thresholds[1] = thres2;

        float fillValue = 0;
        boolean isInverse = true;
        boolean regionFlag = true;


        try {

            // No need to make new image space because the user has choosen to replace the source image
            // Make the algorithm class
            thresholdAlgo = new AlgorithmThresholdDual(image, thresholds, fillValue, outputType, regionFlag,
                    isInverse);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            thresholdAlgo.addListener(this);

            createProgressBar(image.getImageName(), thresholdAlgo);

            // Hide the dialog since the algorithm is about to run.
            setVisible(false);

            thresholdAlgo.run();

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog threshold: unable to allocate enough memory");

            return;
        }
    }

    public void IDobjects() {

        try {

            // No need to make new image space because the user has choosen to replace the source image
            // Make the algorithm class
            idObjectsAlgo2D = new AlgorithmMorphology2D(image, 0, 0, AlgorithmMorphology2D.ID_OBJECTS, 0,
                    0, 0, 0, true);
            idObjectsAlgo2D.setMinMax(5000, 30000);

            idObjectsAlgo2D.addListener(this);

            // Hide the dialog since the algorithm is about to run.
            setVisible(false);

            idObjectsAlgo2D.run();

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog ID objects: unable to allocate enough memory");

            return;
        }
    }

    public void maskToVOI() {
        final AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(image);

        progressBar = new ViewJProgressBar(image.getImageName(), "Extracting VOI ...", 0, 100, true);
        progressBar.setSeparateThread(false);
        VOIExtractionAlgo.addProgressChangeListener(progressBar);
        VOIExtractionAlgo.setProgressValues(0, 100);

        // VOIExtractionAlgo.setActiveImage(false);
        VOIExtractionAlgo.run();

        ScriptRecorder.getReference().addLine(new ActionMaskToVOI(image));
        ProvenanceRecorder.getReference().addLine(new ActionMaskToVOI(image));
        try {
            image.calcMinMax();
            image.notifyImageDisplayListeners(null, true);
        } catch (OutOfMemoryError error) {
            MipavUtil
            .displayError("Out of memory: unable to open new frame");
        }
    }



    public void smoothVOI() {

        int i;
        Vector contours;
        int nVOI, nContours;
        float[] xPoints = null;
        float[] yPoints = null;
        float[] zPoints = null;

        VOIs = image.getVOIs();
        VOIs.VOIAt(0).setActive(true);



        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }


        for (groupNum = 0; groupNum < nVOI; groupNum++) {
            System.out.println("groupNum = " + groupNum);
            System.out.println("Active = " + VOIs.VOIAt(groupNum).isActive());
            System.out.println("Curve type = " + VOIs.VOIAt(groupNum).getCurveType());
            if ((VOIs.VOIAt(groupNum).isActive() == true)
                    && (VOIs.VOIAt(groupNum).getCurveType() == VOI.CONTOUR)) {
                break;
            }
        }


        if (groupNum == nVOI) {
            MipavUtil.displayError("VOI must be selected");
            dispose();

            return;
        }

        voiColor = VOIs.VOIAt(groupNum).getColor();
        contours = VOIs.VOIAt(groupNum).getCurves();
        nContours = contours.size();

        VOIContour activeContour = null;
        for (elementNum = 0; elementNum < nContours; elementNum++) {
            ((VOIContour)(contours.elementAt(elementNum))).setActive(true);
            if (((VOIContour) (contours.elementAt(elementNum)))
                    .isActive()) {
                activeContour = (VOIContour) (contours.elementAt(elementNum));
                break;
            }
        }

        if (elementNum == nContours || activeContour == null) {

            // Don't think this should happen under normal operations
            dispose();
            System.err.println("ruida");
            return;
        }
        System.err.println("ruida pass");

        int npoints = activeContour.size();
        xPoints = new float[npoints + 5];
        yPoints = new float[npoints + 5];
        zPoints = new float[npoints + 5];

        xPoints[0] = activeContour.elementAt(npoints - 2).X;
        yPoints[0] = activeContour.elementAt(npoints - 2).Y;
        zPoints[0] = activeContour.elementAt(npoints - 2).Z;

        xPoints[1] = activeContour.elementAt(npoints - 1).X;
        yPoints[1] = activeContour.elementAt(npoints - 1).Y;
        zPoints[1] = activeContour.elementAt(npoints - 1).Z;

        for (i = 0; i < npoints; i++) {
            xPoints[i + 2] = activeContour.elementAt(i).X;
            yPoints[i + 2] = activeContour.elementAt(i).Y;
            zPoints[i + 2] = activeContour.elementAt(i).Z;
        }

        xPoints[npoints + 2] = activeContour.elementAt(0).X;
        yPoints[npoints + 2] = activeContour.elementAt(0).Y;
        zPoints[npoints + 2] = activeContour.elementAt(0).Z;

        xPoints[npoints + 3] = activeContour.elementAt(1).X;
        yPoints[npoints + 3] = activeContour.elementAt(1).Y;
        zPoints[npoints + 3] = activeContour.elementAt(1).Z;

        xPoints[npoints + 4] = activeContour.elementAt(2).X;
        yPoints[npoints + 4] = activeContour.elementAt(2).Y;
        zPoints[npoints + 4] = activeContour.elementAt(2).Z;

        AlgorithmArcLength arcLength = new AlgorithmArcLength(xPoints, yPoints, zPoints);
        defaultPts = Math.round(arcLength.getTotalArcLength() / 3);

        try {

            // No need to make new image space because the user has choosen to
            // replace the source image
            // Make the algorithm class
            smoothAlgo = new AlgorithmBSmooth(image, VOIs.VOIAt(groupNum),
                    defaultPts, trim);

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed of failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            smoothAlgo.addListener(this);

            // Hide the dialog since the algorithm is about to run.
            setVisible(false);

            // Start the thread as a low priority because we wish to still have
            // user interface.
            /*
			if (smoothAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil
						.displayError("A thread is already running on this object");
			}
             */
            smoothAlgo.run();
        } catch (OutOfMemoryError x) {
            MipavUtil
            .displayError("Dialog Smooth: unable to allocate enough memory");

            return;
        }
    }

    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmThresholdDual) {
            image.clearMask();

            if ((thresholdAlgo.isCompleted() == true)) {

                // The algorithm has completed and produced a new image to be
                // displayed.
                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE)
                        || (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
                    updateFileInfoOtherModality(image, image);
                } else {
                    updateFileInfo(image, image);
                }

                try {
                    image.calcMinMax();
                    image.notifyImageDisplayListeners(null, true);
                } catch (OutOfMemoryError error) {
                    MipavUtil
                    .displayError("Out of memory: unable to open new frame");
                }
            }
            thresholdAlgo.finalize();
            thresholdAlgo = null;

        } else if (algorithm instanceof AlgorithmMorphology2D) {
            image.clearMask();

            if ((particleAlgo2D.isCompleted() == true) || idObjectsAlgo2D.isCompleted() == true ) {
                updateFileInfo(image, image);

            }
            try {
                image.calcMinMax();
                image.notifyImageDisplayListeners(null, true);
            } catch (OutOfMemoryError error) {
                MipavUtil
                .displayError("Out of memory: unable to open new frame");
            }
        } else if (algorithm instanceof AlgorithmBSmooth) {

            VOI resultVOI;
            int element;
            Vector contours;
            int nContours;
            boolean removeOriginal = true;

            if (smoothAlgo.isCompleted() == true) {

                // The algorithm has completed and produced a
                resultVOI = smoothAlgo.getResultVOI();

                if (removeOriginal) {
                    resultVOI.setColor(voiColor);
                    resultVOI.setAllActive(true);

                    contours = VOIs.VOIAt(groupNum).getCurves();

                    int resultIndex = 0;

                    nContours = contours.size();
                    resultIndex = 0;
                    for (element = nContours - 1; element >= 0; element--) {

                        if (((VOIContour) (contours.elementAt(element))).isActive()) {
                            //System.err.println("slice is: " + slice + " element is: " + element + " groupnum is: " +
                            //groupNum);
                            contours.removeElementAt(element);

                            VOIs.VOIAt(groupNum).importCurve((VOIContour) resultVOI.getCurves().elementAt(resultIndex++));
                            //  VOIs.VOIAt(groupNum).importCurve((VOIContour) resultVOI.getActiveContour(slice), slice);
                        }
                    }

                    try {
                        image.importData(0, originalBuffer, true);
                        image.calcMinMax();
                        image.notifyImageDisplayListeners(null, true);
                    } catch (IOException error) {
                        MipavUtil
                        .displayError("I/O error: unable to import original buffer");
                    }
                } else {
                    image.registerVOI(resultVOI);
                    // System.err.println("would have registered the new one here");
                }
            }

        }


        dispose();
    }




}