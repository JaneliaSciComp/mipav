package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.*;
import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.awt.event.ActionEvent;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;

/**
 *   Dialog to transform an image in the tri planar view based on a bounding box.
 */
public class JDialogTriImageTransformation extends JDialogBase
{
    /** Image to transform.*/
    private ModelImage imageA;

    /** Image B to transform, can be null. */
    private ModelImage imageB;

    /** Pointer back to frame that called this. */
    private ViewJFrameTriImage frame;

    /** Progress bar. */
    private ViewJProgressBar progressBar;

    private JRadioButton replaceImage;
    private JRadioButton newImage;
    private JComboBox comboBoxInterp;
    private boolean doNew;

    private double thetaXY, thetaXZ, thetaZY; // rotation angles in degrees, the negative of these
    // angles is passed to setRotate
    private double centerX, centerY, centerZ; // centers of original images
    private int boxIndex = 0; // index from interpolation combo box
    private double xfrmD[][] = new double[4][4]; // (xfrm.inverse()).getArray()


    /**
     *   Creates confirmation dialog for cropping triplanar image.
     *   @param theParentFrame   Pointer to the frame that created this dialog.
     *   @param imA              Image A to be transformed.
     *   @param imB              Image B to be transformed (can be null).
     */
    public JDialogTriImageTransformation(ViewJFrameTriImage theParentFrame, ModelImage imA, ModelImage imB)
    {
        super(theParentFrame, true);
        frame = theParentFrame;
        imageA = imA;
        imageB = imB;
        centerX = frame.getCenter()[0];
        centerY = frame.getCenter()[1];
        centerZ = frame.getCenter()[2];
        init();
    }

    /**
     *   Creates confirmation dialog for transforming triplanar image.
     *   @param theParentFrame   Pointer to the frame that created this dialog.
     *   @param im               Image to be transformed.
     */
    public JDialogTriImageTransformation(ViewJFrameTriImage theParentFrame, ModelImage im)
    {
        this(theParentFrame, im, null);
    }


    /**
     *   Initializes GUI components and displays dialog.
     */
    private void init()
    {
        setTitle("Apply transformation matrix");

        JPanel destinationPanel = new JPanel(new GridBagLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        ButtonGroup destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = gbc.WEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0, 0, 0, 0);
        destinationPanel.add(newImage, gbc);
        gbc.gridy = 1;
        destinationPanel.add(replaceImage, gbc);
        // Only if the image is unlocked can it be replaced.
        if (imageA.getLockStatus() == ModelStorageBase.UNLOCKED)
        {
            replaceImage.setEnabled(true);
        }
        else
        {
            replaceImage.setEnabled(false);
        }

        JPanel interpolationPanel = new JPanel(new GridBagLayout());
        interpolationPanel.setBorder(buildTitledBorder("Interpolation"));

        JLabel labelInterp = new JLabel("Interpolation:");
        labelInterp.setForeground(Color.black);
        labelInterp.setFont(serif12);
        labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp = new JComboBox();
        comboBoxInterp.setFont(serif12);
        comboBoxInterp.setBackground(Color.white);
        comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp.addItem("Trilinear");
        comboBoxInterp.addItem("Bspline 3rd order");
        comboBoxInterp.addItem("Bspline 4th order");
        // comboBoxInterp.addItem("Nearest Neighbor");
        comboBoxInterp.setBounds(100, 20, 120, 30);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        interpolationPanel.add(labelInterp, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        interpolationPanel.add(comboBoxInterp, gbc);

        buildOKButton();
        OKButton.setText("Apply");
        buildCancelButton();

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add(interpolationPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(destinationPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     *  Calls transform methods to transform image if "Apply" is pressed; if "Cancel" is pressed, disposes.
     *  @param event Event that triggered function.
     */
    public void actionPerformed(ActionEvent event)
    {
        String command = event.getActionCommand();
        if (command.equals("Apply"))
        {
            boxIndex = comboBoxInterp.getSelectedIndex();
            doNew = newImage.isSelected();

            dispose();

            final SwingWorker worker = new SwingWorker() {
                public Object construct() {
                    transform(imageA, doNew);
                    if (imageB != null)
                    {
                        transform(imageB, doNew);
                    }

                    return null;
                }
            };

            worker.start();

        }
        else if (command.equals("Cancel"))
        {
            dispose();
        }
    }


    /**
     *   Accessor that returns whether or not a new image was created.
     *   @return <code>true</code> if new image.
     */
    public boolean doNew()
    {
        return doNew;
    }

    /**
     *   Sets the transformation matrix and sends the image data to the appropriate
     *   interpolation routine.
     *   @param image    Image on which to perform the transformation.
     *   @param doNew    Flag indicating if should create new image.
     */
    private void transform(ModelImage image, boolean doNew)
    {
        //if (doNew && (resultImage == null)) {
        ModelImage resultImage = null;
        float[] imgBuffer;
        int bufferSize;

        if (image.isColorImage()) {
            bufferSize = 4 * image.getSliceSize() * image.getExtents()[2];
        }
        else {
            bufferSize = image.getSliceSize() * image.getExtents()[2];
        }

        progressBar = new ViewJProgressBar("Preparing image ...", "Transforming image ...", 0, 100, false, null, null);
        progressBar.setLocation(Toolkit.getDefaultToolkit().getScreenSize().width / 2, 50);
        progressBar.setSeparateThread(!SwingUtilities.isEventDispatchThread());
        MipavUtil.centerOnScreen(progressBar);

        if (doNew) {
            try {
                resultImage = (ModelImage) image.clone();
                resultImage.resetVOIs();
            }
            catch (OutOfMemoryError x) {
                MipavUtil.displayError("ViewJFrameTriImage: Unable to allocate memory for result image");
                if (resultImage != null) {
                    resultImage.disposeLocal();
                    resultImage = null;
                }
            }
        }
        else {
            if (resultImage != null) {
                resultImage.disposeLocal();
                resultImage = null;
            }
        }

        thetaXY = ( (ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.AXIAL_A)).getTheta();
        thetaXZ = ( (ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.CORONAL_A)).getTheta();
        thetaZY = ( (ViewJComponentTriImage) frame.getTriImage(ViewJFrameTriImage.SAGITTAL_A)).getTheta();

        TransMatrix xfrm = new TransMatrix(4);
        xfrm.identity();

        xfrm.setTranslate(centerX * image.getFileInfo()[0].getResolutions()[0],
                          centerY * image.getFileInfo()[0].getResolutions()[1],
                          centerZ * image.getFileInfo()[0].getResolutions()[2]);

        int imageOrient = imageA.getFileInfo(0).getImageOrientation();
        if (imageOrient == FileInfoBase.AXIAL){
            xfrm.setRotate( -thetaZY, -thetaXZ, -thetaXY, TransMatrix.DEGREES);
        }
        else if (imageOrient == FileInfoBase.CORONAL){
            xfrm.setRotate( -thetaXY, -thetaZY, -thetaXZ, TransMatrix.DEGREES);
        }
        else if (imageOrient == FileInfoBase.SAGITTAL){
            xfrm.setRotate( -thetaXZ, -thetaXY, -thetaZY, TransMatrix.DEGREES);
        }
        else
        {
            xfrm.setRotate( -thetaZY, -thetaXZ, -thetaXY, TransMatrix.DEGREES);
        }

        xfrm.setTranslate( - (double) centerX * image.getFileInfo()[0].getResolutions()[0],
                           - (double) centerY * image.getFileInfo()[0].getResolutions()[1],
                           - (double) centerZ * image.getFileInfo()[0].getResolutions()[2]);

        System.gc();
        imgBuffer = new float[bufferSize];

        try
        {
            if (resultImage != null)
            {
                resultImage.exportData(0, bufferSize, imgBuffer);
            }
            else
            {
                image.exportData(0, bufferSize, imgBuffer);
            }
        }
        catch (IOException error)
        {
            MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
        }
        progressBar.setTitle("Transforming image ...");
        progressBar.setVisible(true);
        if (image.getNDims() == 3)
        {
            if (boxIndex == 0)
            {
                if (!image.isColorImage())
                {
                    image = doTrilinear(image, resultImage, imgBuffer, xfrm, progressBar); // black and white
                }
                else
                {
                    image = AlgorithmTransform.transformTrilinearC(image, resultImage, imgBuffer, xfrm, xfrmD, progressBar); // color
                    if (doNew)
                    {
                        new ViewJFrameImage(image, null, new Dimension(610, 200), image.getLogMagDisplay());
                    }
                }
            }
            else if (boxIndex == 1)
            {
                if (!image.isColorImage())
                {
                    image = AlgorithmTransform.bspline(image, resultImage, 3, xfrm, progressBar); // black and white
                    if (doNew)
                    {
                        new ViewJFrameImage(image, null, new Dimension(610, 200), image.getLogMagDisplay());
                    }
                    image.calcMinMax();
                }
                else
                {
                    image = AlgorithmTransform.bsplineC(image, resultImage, 3, xfrm, progressBar); // color
                    if (doNew)
                    {
                        new ViewJFrameImage(image, null, new Dimension(610, 200), image.getLogMagDisplay());
                    }
                }
            }
            else if (boxIndex == 2)
            {
                if (!image.isColorImage())
                {
                    image = AlgorithmTransform.bspline(image, resultImage, 4, xfrm, progressBar); // black and white
                    if (doNew)
                    {
                        new ViewJFrameImage(image, null, new Dimension(610, 200), image.getLogMagDisplay());
                    }
                }
                else
                {
                    image = AlgorithmTransform.bsplineC(image, resultImage, 4, xfrm, progressBar); // color
                    if (doNew)
                    {
                        new ViewJFrameImage(image, null, new Dimension(610, 200), image.getLogMagDisplay());
                    }
                }
            }
        } // if (image.getNDims() == 3)
        else if (image.getNDims() == 4)
        {
            if (boxIndex == 0)
            {
                if (!image.isColorImage())
                {
                    image = AlgorithmTransform.transformTrilinear4D(image, resultImage, xfrm, progressBar); // black and white
                    if (doNew)
                    {
                        new ViewJFrameImage(image, null, new Dimension(610, 200), image.getLogMagDisplay());
                    }
                }
                else
                {
                    image = AlgorithmTransform.transformTrilinearC4D(image, resultImage, xfrm, progressBar); // color
                    if (doNew)
                    {
                        new ViewJFrameImage(image, null, new Dimension(610, 200), image.getLogMagDisplay());
                    }
                }
            }
            else if (boxIndex == 1)
            {
                if (!image.isColorImage())
                {
                    image = AlgorithmTransform.bspline4D(image, resultImage, 3, xfrm, progressBar); // black and white
                    if (doNew)
                    {
                        new ViewJFrameImage(image, null, new Dimension(610, 200), image.getLogMagDisplay());
                    }
                }
                else
                {
                    image = AlgorithmTransform.bsplineC4D(image, resultImage, 3, xfrm, progressBar); // color
                    if (doNew)
                    {
                        new ViewJFrameImage(image, null, new Dimension(610, 200), image.getLogMagDisplay());
                    }
                }
            }
            else if (boxIndex == 2)
            {
                if (!image.isColorImage())
                {
                    image = AlgorithmTransform.bspline4D(image, resultImage, 4, xfrm, progressBar); // black and white
                    if (doNew)
                    {
                        new ViewJFrameImage(image, null, new Dimension(610, 200), image.getLogMagDisplay());
                    }
                }
                else
                {
                    image = AlgorithmTransform.bsplineC4D(image, resultImage, 4, xfrm, progressBar); // color
                    if (doNew)
                    {
                        new ViewJFrameImage(image, null, new Dimension(610, 200), image.getLogMagDisplay());
                    }
                }
            }
        } // else if (image.getNDims == 4)
    }

    /**
     *   Performs trilinear interpolation on black and white image data
     *   @param image        Image from which the data is derived
     *   @param resultImage  Image to put result in; can be null.
     *   @param imgBuffer    Buffer containing image data.
     *   @param xfrm         Transformation to apply.
     */
    public static ModelImage doTrilinear(ModelImage image,
                                                ModelImage resultImage,
                                                float[] imgBuffer,
                                                TransMatrix xfrm,
                                                ViewJProgressBar progressBar)
    {

        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];
        int zDim = image.getExtents()[2];

        float xRes = image.getFileInfo(0).getResolutions()[0];
        float yRes = image.getFileInfo(0).getResolutions()[1];
        float zRes = image.getFileInfo(0).getResolutions()[2];

        if (resultImage != null)
        {
            AlgorithmTransform.transformTrilinear(imgBuffer, resultImage, xfrm,
                                                  xDim, yDim, zDim, xRes, yRes, zRes,
                                                  progressBar, true);
        }
        else
        {
            AlgorithmTransform.transformTrilinear(imgBuffer, image, xfrm,
                                                  xDim, yDim, zDim, xRes, yRes, zRes,
                                                  progressBar, true);
        }

        if (resultImage != null)
        {
            resultImage.calcMinMax();
        }
        else
        {
            image.calcMinMax();
        }

        new ViewJFrameImage(resultImage, null, new Dimension(610, 200), image.getLogMagDisplay());
        return image;
    }



    public JPanel getMainPanel()
    {
        setTitle("Apply transformation matrix");

        JPanel destinationPanel = new JPanel(new GridBagLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        ButtonGroup destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = gbc.WEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0, 0, 0, 0);
        destinationPanel.add(newImage, gbc);
        gbc.gridy = 1;
        destinationPanel.add(replaceImage, gbc);
        // Only if the image is unlocked can it be replaced.
        if (imageA.getLockStatus() == ModelStorageBase.UNLOCKED)
        {
            replaceImage.setEnabled(true);
        }
        else
        {
            replaceImage.setEnabled(false);
        }

        JPanel interpolationPanel = new JPanel(new GridBagLayout());
        interpolationPanel.setBorder(buildTitledBorder("Interpolation"));

        JLabel labelInterp = new JLabel("Interpolation:");
        labelInterp.setForeground(Color.black);
        labelInterp.setFont(serif12);
        labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp = new JComboBox();
        comboBoxInterp.setFont(serif12);
        comboBoxInterp.setBackground(Color.white);
        comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp.addItem("Trilinear");
        comboBoxInterp.addItem("Bspline 3rd order");
        comboBoxInterp.addItem("Bspline 4th order");
        // comboBoxInterp.addItem("Nearest Neighbor");
        comboBoxInterp.setBounds(100, 20, 120, 30);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        interpolationPanel.add(labelInterp, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        interpolationPanel.add(comboBoxInterp, gbc);

        buildOKButton();
        OKButton.setText("Apply");
        // buildCancelButton();

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        // buttonPanel.add(cancelButton);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add(interpolationPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(destinationPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel panel = new JPanel();
        // panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.add(mainPanel);
        panel.add(buttonPanel, BorderLayout.SOUTH);
        // pack();
        return panel;
    }

}
