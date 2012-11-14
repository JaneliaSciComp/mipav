package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get scale factor and interpolation method to create initial image forwarded to ViewJFrameAnimate. The
 * interpolation is done on a slice by slice basis only. Bilinear or bspline may be chosen. For 4 dimensional images the
 * user also enters the number of rows and columns for the different z slices, where z is the third dimension and
 * animation is performed on the fourth time dimension. For 4D images the user can select whether or not to have a frame
 * border around each z image and the color of the frame border.
 */
public class JDialogAnimate extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3763989468823480229L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmTransform algoTrans = null;

    /** DOCUMENT ME! */
    private AlgorithmTransform algoTransB = null;

    /** false if column radio button selected. */
    private JButton borderB;

    /** DOCUMENT ME! */
    private Color borderCol = Color.red; // default frame border color for 4D images

    /** DOCUMENT ME! */
    private ViewJColorChooser colorChooser = null;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp;

    /** DOCUMENT ME! */
    private ViewJFrameBase controlFrame = null;

    /** DOCUMENT ME! */
    private int DIM;

    /** DOCUMENT ME! */
    private ViewJFrameAnimate frameAnimate;

    /** DOCUMENT ME! */
    private JCheckBox frameBorderCheckBox;

    /** DOCUMENT ME! */
    private ModelImage imageA; // source imageA

    /** DOCUMENT ME! */
    private ModelImage imageB; // source imageB

    /** Is dialog visible or not. */
    private boolean isVisible = true;

    /** DOCUMENT ME! */
    private ModelLUT LUTa = null;

    /** DOCUMENT ME! */
    private ModelLUT LUTb = null;

    /** DOCUMENT ME! */
    private JRadioButton radioColumn;

    /** DOCUMENT ME! */
    private JRadioButton radioRow;

    /** DOCUMENT ME! */
    private JTextField rcText;

    /** DOCUMENT ME! */
    private ModelImage resultImageA = null; // image produced by scaling

    /** DOCUMENT ME! */
    private ModelImage resultImageB = null; // image produced by scaling

    /** DOCUMENT ME! */
    private ModelRGB RGBTA = null;

    /** DOCUMENT ME! */
    private ModelRGB RGBTB = null;

    /** each z slice in 4D images. */
    private boolean rowBFlag; // true if row radio button selected,

    /** DOCUMENT ME! */
    private JTextField scaleText;

    /** DOCUMENT ME! */
    private boolean showFrameBorder = true; // tells whether or not to show borders around

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogAnimate object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  _imageA         Source image A.
     * @param  LUTa            Look up table for image A.
     * @param  _imageB         Source image B.
     * @param  LUTb            Look up table for image B.
     */
    public JDialogAnimate(Frame theParentFrame, ModelImage _imageA, ModelLUT LUTa, ModelImage _imageB, ModelLUT LUTb) {
        super(theParentFrame, false);
        this.LUTa = LUTa;
        this.LUTb = LUTb;
        imageA = _imageA;
        imageB = _imageB;

        Vector<ViewImageUpdateInterface> frameList = imageA.getImageFrameVector();

        for (int i = 0; i < frameList.size(); i++) {

            if ((((ViewJFrameBase) frameList.elementAt(i)).getControls()) != null) {
                controlFrame = ((ViewJFrameBase) frameList.elementAt(i));
            }
        }

        if (imageA.getNDims() == 3) {
            DIM = 3;
        } else if (imageA.getNDims() == 4) {
            DIM = 4;
        }

        if ((DIM == 3) && (controlFrame.getControls() != null)) {
            borderCol = controlFrame.getControls().getTools().getPaintColor();
        }

        if ((imageB != null) && (imageB.getNDims() == 4)) {
            DIM = 4;
        }

        init();
    }

    /**
     * Creates a new JDialogAnimate object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  _imageA         Source image A.
     * @param  LUTa            Look up table for image A.
     * @param  _imageB         Source image B.
     * @param  LUTb            Look up table for image B.
     * @param  _isVisible      DOCUMENT ME!
     */
    public JDialogAnimate(Frame theParentFrame, ModelImage _imageA, ModelLUT LUTa, ModelImage _imageB, ModelLUT LUTb,
                          boolean _isVisible) {
        super(theParentFrame, false);
        this.LUTa = LUTa;
        this.LUTb = LUTb;
        imageA = _imageA;
        imageB = _imageB;

        controlFrame = (ViewJFrameBase) theParentFrame;

        if (imageA.getNDims() == 3) {
            DIM = 3;
        } else if (imageA.getNDims() == 4) {
            DIM = 4;
        }

        if ((DIM == 3) && (controlFrame.getControls() != null)) {
            borderCol = controlFrame.getControls().getTools().getPaintColor();
        }

        if ((imageB != null) && (imageB.getNDims() == 4)) {
            DIM = 4;
        }

        isVisible = _isVisible;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Responds to row button, column button, OK button, cancel button, and borderB button for color. OK button
     * processes images and calls ViewJFrameAnimate.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();

        if (source == radioRow) {
            rowBFlag = true;
        } else if (source == radioColumn) {
            rowBFlag = false;
        } else if ((source == OKButton) || command.equals("OK")) {
            animate();
        } else if (source == cancelButton) {
            imageA = null;
            imageB = null;
            resultImageA = null;
            resultImageB = null;
            algoTrans = null;
            algoTransB = null;

            dispose();
        } else if (source == borderB) {

            // Choose the color of the frame surrounding each z slice
            colorChooser = new ViewJColorChooser(parentFrame, "Pick border color", new OkBorderListener(),
                                                 new CancelListener());
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Invoke the animation frame.
     */
    public void animate() {
        String tmpStr;
        double Sx, Sy;

        System.gc();

        ModelImage transformedImageA = null, transformedImageB = null;
        int nRow = 1, nColumn = 1;
        float oXres, oYres;
        float iXres, iYres;
        int oXdim, oYdim;
        int iXdim, iYdim, iZdim, iTdim;
        TransMatrix xfrm = new TransMatrix(3);
        int boxIndex = 0;
        int interp = 0;

        xfrm.MakeIdentity();

        // Hide dialog
        setVisible(false);

        iXres = imageA.getFileInfo(0).getResolutions()[0];
        iYres = imageA.getFileInfo(0).getResolutions()[1];
        iXdim = imageA.getExtents()[0];
        iYdim = imageA.getExtents()[1];
        iZdim = imageA.getExtents()[2];
        iTdim = 1;

        if (DIM == 4) {

            if (imageA.getNDims() == 4) {
                iTdim = imageA.getExtents()[3];
            } else {
                iTdim = imageB.getExtents()[3];
            }
        }

        oXdim = iXdim;
        oYdim = iYdim;
        oXres = iXres;
        oYres = iYres;

        // Obtain the scale factor by which to scale each 2D slice of 3D and 4D images
        // The x and y scalings are identical.
        tmpStr = scaleText.getText();

        if (testParameter(tmpStr, 0.001, 300)) {
            Sx = Double.valueOf(tmpStr).doubleValue();
            Sy = Sx;
        } else {
            scaleText.requestFocus();
            scaleText.selectAll();

            return;
        }

        if (DIM == 4) {
            tmpStr = rcText.getText();

            if (testParameter(tmpStr, 1, iZdim)) {

                // If the row radio button is selected, then the text entered in rcText is taken as
                // the number of rows.  The number of columns is then calculated from the number of
                // z slices and the number of rows.
                if (rowBFlag) {
                    nRow = Integer.valueOf(tmpStr).intValue();
                    nColumn = iZdim / nRow;

                    if ((iZdim % nRow) > 0) {
                        nColumn = nColumn + 1;
                    }
                } else {

                    // If the column radio button is selected, then the text entered in rcText is taken as
                    // the number of columns.  The number of rows is then calculated from the number of
                    // z slices and the number of columns.
                    nColumn = Integer.valueOf(tmpStr).intValue();
                    nRow = iZdim / nColumn;

                    if ((iZdim % nColumn) > 0) {
                        nRow = nRow + 1;
                    }
                }
            } else {
                rcText.requestFocus();
                rcText.selectAll();

                return;
            }

            if (frameBorderCheckBox.isSelected()) {

                // The frameBorderCheckBox tells whether or not to show frame borders around each z slice.
                showFrameBorder = true;
            } else {
                showFrameBorder = false;
            }
        } // end of if (DIM == 4)

        if (Sx != 1.0) {

            // If the scale factor is not unity, perform a slice by slice
            // interpolation on the 3D or 4D image
            oXdim = (int) ((Sx * iXdim) + 0.5);
            oYdim = (int) ((Sy * iYdim) + 0.5);
            oXres = (float) Sx * ((float) (iXdim - 1) * iXres) / ((float) (oXdim - 1));
            oYres = (float) Sy * ((float) (iYdim - 1) * iYres) / ((float) (oYdim - 1));

            // Set the transformation matrix scale factors, here Sx = Sy.
            xfrm.setZoom(Sx, Sy);

           // System.out.println(xfrm);

            boxIndex = comboBoxInterp.getSelectedIndex();

            // if (boxIndex == 0)
            // interp = AlgorithmTransform.NEAREST_NEIGHBOR;
            if (boxIndex == 0) {
                interp = AlgorithmTransform.BILINEAR;
            } else if (boxIndex == 1) {
                interp = AlgorithmTransform.BSPLINE3;
            } else if (boxIndex == 2) {
                interp = AlgorithmTransform.BSPLINE4;
            }

            try {

                // AlgorithmTransform scales the image with the selected interpolation method.
                algoTrans = new AlgorithmTransform(imageA, xfrm, interp, oXres, oYres, oXdim, oYdim, false, true,
                                                   false);
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Transform: unable to allocate enough memory");

                return;
            }

            algoTrans.run();

            // ViewJFrameImage imageFrame = null;
            resultImageA = algoTrans.getTransformedImage();
            algoTrans.finalize();

            if (resultImageA == null) {
                MipavUtil.displayError("resultImageA null returned by AlgorithmTransform");

                return;
            }

            resultImageA.calcMinMax();
            resultImageA.setImageName(imageA.getImageName());

            // Update frames
            imageA.notifyImageDisplayListeners(null, true);

            if (imageB != null) {

                try {

                    // AlgorithmTransform scales the image with the selected interpolation method.
                    algoTransB = new AlgorithmTransform(imageB, xfrm, interp, oXres, oYres, oXdim, oYdim, false, true,
                                                        false);
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Transform: unable to allocate enough memory");

                    return;
                }

                algoTransB.run();

                // ViewJFrameImage imageFrameB = null;
                resultImageB = algoTransB.getTransformedImage();
                algoTransB.finalize();

                if (resultImageB == null) {
                    MipavUtil.displayError("resultImageB null returned by AlgorithmTransform");

                    return;
                }

                resultImageB.calcMinMax();
                resultImageB.setImageName(imageA.getImageName());

                // Update frames
                imageB.notifyImageDisplayListeners(null, true);
            } // end of if (imageB != null)
        } // end of if (Sx != 1.0)

        if (DIM == 4) {

            // convert the unscaled or scaled 4D image to a 3D image
            // Each z image will have a 3 pixel width color frame around the border and be
            // separated from the neighboring z image by 3 black pixels, followed by the
            // 3 colored pixels, followed by 3 black pixels.
            int oldOXdim = oXdim;
            int oldOYdim = oYdim;
            oXdim = (oXdim * nColumn) + (6 * nColumn) + (3 * (nColumn - 1));
            oYdim = (oYdim * nRow) + (6 * nRow) + (3 * (nRow - 1));

            int[] extents = new int[] { oXdim, oYdim, iTdim };
            float[] resolutions;

            if (imageA.getNDims() == 4) {
                resolutions = new float[] { oXres, oYres, imageA.getFileInfo(0).getResolutions()[3] };
            } else {
                resolutions = new float[] { oXres, oYres, imageB.getFileInfo(0).getResolutions()[3] };
            }

            String name = makeImageName(imageA.getImageName(), "_result");

            transformedImageA = new ModelImage(imageA.getType(), extents, name);

            FileInfoBase[] fileInfo = transformedImageA.getFileInfo();

            for (int i = 0; i < iTdim; i++) {
                fileInfo[i].setModality(imageA.getFileInfo()[0].getModality());
                fileInfo[i].setFileDirectory(imageA.getFileInfo()[0].getFileDirectory());
                fileInfo[i].setDataType(imageA.getFileInfo()[0].getDataType());
                fileInfo[i].setEndianess(imageA.getFileInfo()[0].getEndianess());
                fileInfo[i].setUnitsOfMeasure(imageA.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[i].setResolutions(resolutions);
                fileInfo[i].setExtents(extents);
                fileInfo[i].setPixelPadValue(imageA.getFileInfo()[0].getPixelPadValue());
                fileInfo[i].setPhotometric(imageA.getFileInfo()[0].getPhotometric());
            }

            if (imageB != null) {

                if (imageB.getNDims() == 4) {
                    resolutions = new float[] { oXres, oYres, imageB.getFileInfo(0).getResolutions()[3] };
                } else {
                    resolutions = new float[] { oXres, oYres, imageA.getFileInfo(0).getResolutions()[3] };
                }

                name = makeImageName(imageB.getImageName(), "_result");

                transformedImageB = new ModelImage(imageB.getType(), extents, name);

                fileInfo = transformedImageB.getFileInfo();

                for (int i = 0; i < iTdim; i++) {
                    fileInfo[i].setModality(imageB.getFileInfo()[0].getModality());
                    fileInfo[i].setFileDirectory(imageB.getFileInfo()[0].getFileDirectory());
                    fileInfo[i].setDataType(imageB.getFileInfo()[0].getDataType());
                    fileInfo[i].setEndianess(imageB.getFileInfo()[0].getEndianess());
                    fileInfo[i].setUnitsOfMeasure(imageB.getFileInfo()[0].getUnitsOfMeasure());
                    fileInfo[i].setResolutions(resolutions);
                    fileInfo[i].setExtents(extents);
                    fileInfo[i].setPixelPadValue(imageB.getFileInfo()[0].getPixelPadValue());
                    fileInfo[i].setPhotometric(imageB.getFileInfo()[0].getPhotometric());
                }
            } // end of if (imageB != null)

            int sliceSize = oldOXdim * oldOYdim;
            int bufferFactor = 1;

            if (imageA.isColorImage()) {
                bufferFactor = 4;
                sliceSize = sliceSize * 4;
            }

            int m, colNum, rowNum;
            float[] imgBuf = new float[sliceSize];

            if (bufferFactor == 1) { // black and white image

                for (int l = 0; l < iTdim; l++) { // Go thru the 4th time dimension

                    if (imageA.getNDims() == 4) {
                        m = l;
                    } else {
                        m = 0;
                    }

                    for (int k = 0; k < iZdim; k++) { // Go thru the 3rd z dimension
                        colNum = k % nColumn;
                        rowNum = k / nColumn;

                        try {

                            if (Sx == 1.0) { // the image was not scaled so use original imageA

                                // to export each 2D slice to imgBuf
                                imageA.exportData((m * iZdim * sliceSize) + (k * sliceSize), sliceSize, imgBuf);
                            } else { // the image was scaled so used resultImageA

                                // to export each 2D slice to imgBuf
                                resultImageA.exportData((m * iZdim * sliceSize) + (k * sliceSize), sliceSize, imgBuf);
                            }
                        } catch (IOException error) {
                            MipavUtil.displayError("Error on exportData in JDialogAnimate");

                            return;
                        }

                        for (int i = 0; i < oldOXdim; i++) {

                            for (int j = 0; j < oldOYdim; j++) {

                                // Set the a pixel in the newly created 3D transformedImageA
                                // to the corresponding value in imgBuf
                                transformedImageA.set(i + 3 + ((oldOXdim + 9) * colNum),
                                                      j + 3 + ((oldOYdim + 9) * rowNum), l, imgBuf[i + (j * oldOXdim)]);
                            } // end of for (j = 0; j < oldYdim; j++)
                        } // end of for (i = 0; i < oldOXdim; i++)
                    } // end of for (k = 0; k < iZdim; k++)
                } // end of for (l = 0; l < iTdim; l++)
            } // end of if (bufferFactor == 1)
            else { // bufferFactor == 4 for a color image

                for (int l = 0; l < iTdim; l++) { // Go thru the 4th time dimension

                    if (imageA.getNDims() == 4) {
                        m = l;
                    } else {
                        m = 0;
                    }

                    for (int k = 0; k < iZdim; k++) { // GO thru the 3rd z dimension
                        colNum = k % nColumn;
                        rowNum = k / nColumn;

                        try {

                            if (Sx == 1.0) { // the image was not scaled so use original imageA

                                // to export each 2D slice to imgBuf
                                imageA.exportData((m * iZdim * sliceSize) + (k * sliceSize), sliceSize, imgBuf);
                            } else { // the image was scaled so used resultImageA

                                // to export each 2D slice to imgBuf
                                resultImageA.exportData((m * iZdim * sliceSize) + (k * sliceSize), sliceSize, imgBuf);
                            }
                        } catch (IOException error) {
                            MipavUtil.displayError("Error on exportData in JDialogAnimate");

                            return;
                        }

                        for (int i = 0; i < oldOXdim; i++) {

                            for (int j = 0; j < oldOYdim; j++) {

                                // Set the ARGB values in the newly created 3D transformedImageA
                                // to the corresponding values in imgBuf
                                transformedImageA.setC(i + 3 + ((oldOXdim + 9) * colNum),
                                                       j + 3 + ((oldOYdim + 9) * rowNum), l, 0,
                                                       imgBuf[4 * (i + (j * oldOXdim))]);
                                transformedImageA.setC(i + 3 + ((oldOXdim + 9) * colNum),
                                                       j + 3 + ((oldOYdim + 9) * rowNum), l, 1,
                                                       imgBuf[(4 * (i + (j * oldOXdim))) + 1]);
                                transformedImageA.setC(i + 3 + ((oldOXdim + 9) * colNum),
                                                       j + 3 + ((oldOYdim + 9) * rowNum), l, 2,
                                                       imgBuf[(4 * (i + (j * oldOXdim))) + 2]);
                                transformedImageA.setC(i + 3 + ((oldOXdim + 9) * colNum),
                                                       j + 3 + ((oldOYdim + 9) * rowNum), l, 3,
                                                       imgBuf[(4 * (i + (j * oldOXdim))) + 3]);
                            } // end of for (j = 0; j < oldYdim; j++)
                        } // end of for (i = 0; i < oldOXdim; i++)
                    } // end of for (k = 0; k < iZdim; k++)
                } // end of for (l = 0; l < iTdim; l++)
            } // end of else bufferFactor == 4

            transformedImageA.calcMinMax();
            transformedImageA.setImageName(imageA.getImageName());

            if (imageB != null) {

                if (bufferFactor == 1) { // black and white image

                    for (int l = 0; l < iTdim; l++) { // go thru the fourth time dimension

                        if (imageB.getNDims() == 4) {
                            m = l;
                        } else {
                            m = 0;
                        }

                        for (int k = 0; k < iZdim; k++) { // go thru the 3rd z dimension
                            colNum = k % nColumn;
                            rowNum = k / nColumn;

                            try {

                                if (Sx == 1.0) { // the image was not scaled so use original imageB

                                    // to export each 2D slice to imgBuf
                                    imageB.exportData((m * iZdim * sliceSize) + (k * sliceSize), sliceSize, imgBuf);
                                } else { // the image was scaled so used resultImageB

                                    // to export each 2D slice to imgBuf
                                    resultImageB.exportData((m * iZdim * sliceSize) + (k * sliceSize), sliceSize,
                                                            imgBuf);
                                }
                            } catch (IOException error) {
                                MipavUtil.displayError("Error on exportData in JDialogAnimate");

                                return;
                            }

                            for (int i = 0; i < oldOXdim; i++) {

                                for (int j = 0; j < oldOYdim; j++) {

                                    // Set the a pixel in the newly created 3D transformedImageB
                                    // to the corresponding value in imgBuf
                                    transformedImageB.set(i + 3 + ((oldOXdim + 9) * colNum),
                                                          j + 3 + ((oldOYdim + 9) * rowNum), l,
                                                          imgBuf[i + (j * oldOXdim)]);
                                } // end of for (j = 0; j < oldYdim; j++)
                            } // end of for (i = 0; i < oldOXdim; i++)
                        } // end of for (k = 0; k < iZdim; k++)
                    } // end of for (l = 0; l < iTdim; l++)
                } // end of if (bufferFactor == 1)
                else { // bufferFactor == 4 for color

                    for (int l = 0; l < iTdim; l++) { // go thru the 4th time dimension

                        if (imageB.getNDims() == 4) {
                            m = l;
                        } else {
                            m = 0;
                        }

                        for (int k = 0; k < iZdim; k++) { // go thru the 3rd z dimension
                            colNum = k % nColumn;
                            rowNum = k / nColumn;

                            try {

                                if (Sx == 1.0) { // the image was not scaled so use original imageB

                                    // to export each 2D slice to imgBuf
                                    imageB.exportData((m * iZdim * sliceSize) + (k * sliceSize), sliceSize, imgBuf);
                                } else { // the image was scaled so used resultImageB

                                    // to export each 2D slice to imgBuf
                                    resultImageB.exportData((m * iZdim * sliceSize) + (k * sliceSize), sliceSize,
                                                            imgBuf);
                                }
                            } catch (IOException error) {
                                MipavUtil.displayError("Error on exportData in JDialogAnimate");

                                return;
                            }

                            for (int i = 0; i < oldOXdim; i++) {

                                for (int j = 0; j < oldOYdim; j++) {

                                    // Set the ARGB values in the newly created 3D transformedImageB
                                    // to the corresponding values in imgBuf
                                    transformedImageB.setC(i + 3 + ((oldOXdim + 9) * colNum),
                                                           j + 3 + ((oldOYdim + 9) * rowNum), l, 0,
                                                           imgBuf[4 * (i + (j * oldOXdim))]);
                                    transformedImageB.setC(i + 3 + ((oldOXdim + 9) * colNum),
                                                           j + 3 + ((oldOYdim + 9) * rowNum), l, 1,
                                                           imgBuf[(4 * (i + (j * oldOXdim))) + 1]);
                                    transformedImageB.setC(i + 3 + ((oldOXdim + 9) * colNum),
                                                           j + 3 + ((oldOYdim + 9) * rowNum), l, 2,
                                                           imgBuf[(4 * (i + (j * oldOXdim))) + 2]);
                                    transformedImageB.setC(i + 3 + ((oldOXdim + 9) * colNum),
                                                           j + 3 + ((oldOYdim + 9) * rowNum), l, 3,
                                                           imgBuf[(4 * (i + (j * oldOXdim))) + 3]);
                                } // end of for (j = 0; j < oldYdim; j++)
                            } // end of for (i = 0; i < oldOXdim; i++)
                        } // end of for (k = 0; k < iZdim; k++)
                    } // end of for (l = 0; l < iTdim; l++)
                } // end of else bufferFactor == 4

                transformedImageB.calcMinMax();
                transformedImageB.setImageName(imageB.getImageName());
            } // end of if (imageB != null)
        } // end of if (DIM == 4)

        try {

            if ((Sx == 1.0) && (DIM == 3)) {

                // unscaled 3D image - pass the original imageA and possibly imageB to ViewJFrameAnimate
                frameAnimate = new ViewJFrameAnimate(imageA, LUTa, imageB, LUTb, RGBTA, RGBTB, controlFrame, nRow,
                                                     nColumn, imageA.getExtents()[2], showFrameBorder, borderCol,
                                                     false);

            } else if (DIM == 3) {

                // scaled 3D image - pass the scaled resultImageA and possibly resultImageB to ViewJFrameAnimate
                frameAnimate = new ViewJFrameAnimate(resultImageA, LUTa, resultImageB, LUTb, RGBTA, RGBTB, controlFrame,
                                                     nRow, nColumn, imageA.getExtents()[2], showFrameBorder, borderCol,
                                                     true);
            } else { // DIM == 4

                // unscaled or scaled 4D image.  This has been converted to a 3D image named
                // transformedImageA and possibly transformedImageB.  Pass to ViewJFrameAnimate.
                frameAnimate = new ViewJFrameAnimate(transformedImageA, LUTa, transformedImageB, LUTb, RGBTA, RGBTB,
                                                     controlFrame, nRow, nColumn, imageA.getExtents()[2],
                                                     showFrameBorder, borderCol, true);

                // transformedImageA.registerAnimateFrame(animateFrame);
                // For scaled 4D images the scaling produced resultImageA from imageA and
                // then the 4D to 3D conversion produced transformedImageA from resultImageA
                // There is no longer any need to keep the scaled 4D image, so dispose of it.
                if (resultImageA != null) {
                    resultImageA.disposeLocal();
                }

                if (resultImageB != null) {
                    resultImageB.disposeLocal();
                }
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: unable to open Animate frame.");
        }

        imageA = null;
        imageB = null;
        resultImageA = null;
        resultImageB = null;
        transformedImageA = null;
        transformedImageA = null;
        algoTrans = null;
        algoTransB = null;
        dispose();

    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImageA() {
        return resultImageA;
    }

    /**
     * Invoking the ViewJFrameAnimate saveImageAs action.
     *
     * @param  framesPerSecond  DOCUMENT ME!
     */
    public void invokeSaveImgAs(int framesPerSecond) {
        frameAnimate.setDisposeImages(true);
        frameAnimate.setFramesPerSecond(framesPerSecond);
        frameAnimate.actionPerformed(new ActionEvent(this, 1, "SaveImageAs"));
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  event  Algorithm that caused the event.
     */
    // public void algorithmPerformed(AlgorithmBase algorithm) {
    // }

    /**
     * Method to handle item events - currently unused.
     *
     * @param  event  Event that cause the method to fire.
     */
    public void itemStateChanged(ItemEvent event) { // Object source = event.getSource();
    }

    /**
     * Sets the RGB LUT table for ARGB image A and image B.
     *
     * @param  RGBTA  the new RGB LUT to be applied to image A
     * @param  RGBTB  the new RGB LUT to be applied to image B
     */
    public void setRGBs(ModelRGB RGBTA, ModelRGB RGBTB) {
        this.RGBTA = RGBTA;
        this.RGBTB = RGBTB;
    }

    /**
     * Initializes the dialog box to a certain size and adds the components.
     */
    private void init() {
        setTitle("Animate Parameters");

        JPanel scalePanel = new JPanel(new GridLayout(2, 2));
        scalePanel.setBorder(buildTitledBorder("Scale parameters"));

        JLabel scaleLabel = new JLabel("Scale factor:");
        scaleLabel.setForeground(Color.black);
        scaleLabel.setFont(serif12);
        scalePanel.add(scaleLabel);

        scaleText = new JTextField("1.0");
        scaleText.setFont(serif12);
        scaleText.addFocusListener(this);
        scalePanel.add(scaleText);

        JLabel labelInterp = new JLabel("Interpolation:");
        labelInterp.setForeground(Color.black);
        labelInterp.setFont(serif12);
        scalePanel.add(labelInterp);

        comboBoxInterp = new JComboBox();
        comboBoxInterp.setFont(serif12);
        comboBoxInterp.setBackground(Color.white);
        comboBoxInterp.addItem("Bilinear");
        comboBoxInterp.addItem("Bspline 3rd order");
        comboBoxInterp.addItem("Bspline 4th order");

        // comboBoxInterp.addItem("Nearest Neighbor");
        comboBoxInterp.setSelectedIndex(0);
        scalePanel.add(comboBoxInterp);

        GridBagConstraints gbc = new GridBagConstraints();
        JPanel mainPanel = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(scalePanel, gbc);

        if (DIM == 4) {
            JPanel zPanel = new JPanel(new GridBagLayout());
            zPanel.setBorder(buildTitledBorder("Number of rows/columns"));

            JLabel zDimLabel = new JLabel("Number of Z Slices = " + imageA.getExtents()[2]);
            zDimLabel.setForeground(Color.black);
            zDimLabel.setFont(serif12);

            gbc.gridwidth = 2;
            zPanel.add(zDimLabel, gbc);

            ButtonGroup g1 = new ButtonGroup();
            radioRow = new JRadioButton("Row", true);
            radioRow.setFont(serif12);
            g1.add(radioRow);
            radioRow.addActionListener(this);
            rowBFlag = true;

            gbc.gridx = 0;
            gbc.gridy = 1;
            gbc.gridwidth = 1;
            zPanel.add(radioRow, gbc);

            radioColumn = new JRadioButton("Column", false);
            radioColumn.setFont(serif12);
            g1.add(radioColumn);
            radioColumn.addActionListener(this);
            gbc.gridx = 1;
            zPanel.add(radioColumn, gbc);

            JLabel rcLabel = new JLabel("Number of rows or cols: ");
            rcLabel.setForeground(Color.black);
            rcLabel.setFont(serif12);
            gbc.gridx = 0;
            gbc.gridy = 2;
            zPanel.add(rcLabel, gbc);

            rcText = new JTextField("1");
            rcText.setFont(serif12);
            rcText.addFocusListener(this);
            gbc.gridx = 1;
            zPanel.add(rcText, gbc);

            JPanel frameBorderPanel = new JPanel(new GridBagLayout());
            frameBorderPanel.setBorder(buildTitledBorder("Frame Border"));

            frameBorderCheckBox = new JCheckBox("Show Z image frame borders");
            frameBorderCheckBox.setFont(serif12);
            frameBorderCheckBox.setSelected(true);
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.gridwidth = 2;
            frameBorderPanel.add(frameBorderCheckBox, gbc);

            JLabel l2 = new JLabel("Border Color:");
            l2.setFont(serif12);
            l2.setForeground(Color.black);
            gbc.gridy = 1;
            gbc.gridwidth = 1;
            frameBorderPanel.add(l2, gbc);

            borderB = new JButton();
            borderB.setBackground(borderCol);
            borderB.addActionListener(this);
            borderB.setPreferredSize(new Dimension(100, 20));
            gbc.gridx = 1;
            frameBorderPanel.add(borderB, gbc);

            gbc.gridx = 0;
            gbc.gridy = 1;
            gbc.gridwidth = 1;
            mainPanel.add(zPanel, gbc);
            gbc.gridy = 2;
            mainPanel.add(frameBorderPanel, gbc);
        }

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        mainDialogPanel.add(mainPanel);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();

        if (isVisible) {
            setVisible(true);
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Does nothing at the moment.
     */
    class CancelListener implements ActionListener {

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent e) { }
    }

    /**
     * Pick up the selected color and change the image border color. For use by the color chooser.
     */
    class OkBorderListener implements ActionListener {

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent e) {
            Color color = colorChooser.getColor();
            borderB.setBackground(color);
            borderCol = color;
        }
    }

}
