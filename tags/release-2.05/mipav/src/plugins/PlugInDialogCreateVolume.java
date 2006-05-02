import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog for taking series of images, resorting them into groups, inserting blank slices where necessary, setting the
 * slice thickness and saving a new group of image files.
 */
public class PlugInDialogCreateVolume extends JDialogBase implements AlgorithmInterface, MouseListener {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private PlugInAlgorithmCreateVolume createVolumeAlgo;

    /** private int extents[];. */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel numIPPValue;

    /** DOCUMENT ME! */
    private JTextField numPlanes, root;

    /** DOCUMENT ME! */
    private ModelImage[] resultImageSet;

    /** DOCUMENT ME! */
    private String rootName;

    /** DOCUMENT ME! */
    private float thick, space, gap, newThick, zStart, newZStart;

    /** DOCUMENT ME! */
    private int TNI, numPos, numRepIm, numBlank, newTNI, nPlanes, newNPlanes;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor that creates new dialog for Creating Image Volume and displays it.
     *
     * @param  parent  Parent frame of this dialog.
     * @param  image   the image to process
     */
    public PlugInDialogCreateVolume(Frame parent, ModelImage image) {
        super(parent, true);
        this.image = image;

        // userInterface = ((ViewJFrameBase)(parentFrame)).getUserInterface();
        boolean disactivated = false;

        if (disactivated) {
            MipavUtil.displayError("This option not yet available");

            return;
        } else {
            FileInfoXML fileInfo;
            fileInfo = (FileInfoXML) image.getFileInfo(0).clone();

            // slice thickness, spacing, and origin
            space = fileInfo.getSliceSpacing();
            thick = fileInfo.getResolutions()[2];
            gap = space - thick;
            zStart = fileInfo.getOrigin(2);

            // System.out.println("\nIn original image set, slice thickness is " +thick +" and gap between slices is "
            // +gap +".");
            TNI = image.getExtents()[2];

            /*
             * extents = new int[3]; TNI=extents[2];
             */
            /*
             * for DICOM String tmpString = new String(); tmpString = fileInfo.getValue("0018,0050").toString();
             * thick=Float.valueOf(tmpString).floatValue(); tmpString = fileInfo.getValue("0018,0088").toString();
             * space=Float.valueOf(tmpString).floatValue(); tmpString = fileInfo.getValue("0020,1041").toString();
             * zStart=Float.valueOf(tmpString).floatValue();
             */
            init();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Perfoms the following actions based on the command:<br>
     *
     * <ul>
     *   <li>OK -</li>
     *   <li>Cancel - cleans up and disposes the dialog</li>
     * </ul>
     *
     * @param  event  Event that triggered this function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                getNewInfo();

                // System.out.println("New slice thickness: " +newThick);
                // System.out.println("To create new images, each original image is repeated " +numRepIm
                // +" times and " +numBlank +" blanks are inserted.\n\n");
                generateNewImages();
            }

            dispose();
            System.gc();
        } else if (command.equals("Cancel")) {
            dispose();
            System.gc();
        }
    }

    /**
     * This method is required if the AlgorithmInterface is implemented.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        int pN = 0;

        if (algorithm instanceof PlugInAlgorithmCreateVolume) {

            if (createVolumeAlgo.isCompleted() == true) {
                pN = createVolumeAlgo.getCurrentPosition();

                // System.out.println("Create Volume is completed for position " +(pN));
                if (resultImageSet[pN - 1] != null) {
                    updateFileInfo(pN - 1);
                    saveNewImages(pN - 1);
                    resultImageSet[pN - 1].disposeLocal();
                }
            }
        }
    }

    /**
     * Must be defined since mouse listener was implemented. They do nothing.
     *
     * @param  e  mouse event
     */
    public void mouseClicked(MouseEvent e) { }

    /**
     * Must be defined since mouse listener was implemented. They do nothing.
     *
     * @param  e  mouse event
     */
    public void mouseEntered(MouseEvent e) { }

    /**
     * Must be defined since mouse listener was implemented. They do nothing.
     *
     * @param  e  mouse event
     */
    public void mouseExited(MouseEvent e) { }

    /**
     * Must be defined since mouse listener was implemented. They do nothing.
     *
     * @param  e  mouse event
     */
    public void mouseMoved(MouseEvent e) { }

    /**
     * When the user clicks the mouse out of a text field, resets the variables.
     *
     * @param  e  event that triggers the function
     */
    public void mousePressed(MouseEvent e) {
        boolean tmpBool = setVariables();
    }

    /**
     * Must be defined since mouse listener was implemented. They do nothing.
     *
     * @param  e  mouse event
     */
    public void mouseReleased(MouseEvent e) { }

    /**
     * Call algorithm to copy appropriate images from original, and double where necessary.
     *
     * @param  resultImage  the image to put the result into
     * @param  currentPos   the current position within the volume
     */
    private void callAlgorithm(ModelImage resultImage, int currentPos) {

        try {
            createVolumeAlgo = new PlugInAlgorithmCreateVolume(image, resultImage, currentPos, TNI, numPos, numRepIm,
                                                               numBlank);
            createVolumeAlgo.addListener(this);
            setVisible(false); // Hide dialog
            runInSeparateThread = false;

            if (runInSeparateThread) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (createVolumeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("Create Volume reports: A thread is already running on this object [createVolumeAlgo]");
                }
            } else {
                createVolumeAlgo.setActiveImage(isActiveImage);
                createVolumeAlgo.run(); // don't run in separate thread
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up image memory
                resultImage = null;
            }

            MipavUtil.displayError("Insert Slice reports: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Put method description here.
     */
    private void generateNewImages() {
        int[] destExtents;
        String newName;
        nPlanes = TNI / numPos;
        newNPlanes = nPlanes * (numRepIm + numBlank);
        destExtents = new int[3];
        destExtents = (int[]) image.getExtents().clone();
        destExtents[2] = newNPlanes;
        resultImageSet = new ModelImage[numPos];

        try {

            for (int pN = 1; pN <= numPos; pN++) { // pn is the position number
                newName = rootName + pN;
                resultImageSet[pN - 1] = new ModelImage(image.getType(), destExtents, newName,
                                                        image.getUserInterface());
                callAlgorithm(resultImageSet[pN - 1], pN);
            }
        } catch (OutOfMemoryError e) {
            resultImageSet = null;
            MipavUtil.displayError("Create Volume reports: unable to allocate enough memory");

            return;
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void getNewInfo() {
        float sign = 1.f;

        if (zStart < 0) {
            sign = -1.f;
        }

        if (gap == 0.0) {
            newThick = thick;
            numRepIm = 1;
            numBlank = 0;
            newTNI = TNI;
            newZStart = zStart;
        } else if (gap > 0) {

            if (((gap / thick) % 1) == 0.0) {
                newThick = thick;
                numRepIm = 1;
                numBlank = 1 * (int) (gap / thick);
                newTNI = (int) (1 + (gap / thick)) * TNI;
                newZStart = zStart;
            } else if (((thick / gap) % 1) == 0.0) {
                newThick = gap;
                numRepIm = 1 * (int) (thick / gap);
                numBlank = 1;
                newTNI = (int) (1 + (thick / gap)) * TNI;
                newZStart = zStart + (sign * ((-thick / 2.f) + (gap / 2.f)));
            } else if ((2 * (gap / thick) % 1) == 0.0) {
                newThick = thick / 2;
                numRepIm = 2;
                numBlank = 2 * (int) (gap / thick);
                newTNI = (int) (2 + (2 * gap / thick)) * TNI;
                newZStart = zStart + (sign * (-thick / 4.f));
            } else if ((2 * (thick / gap) % 1) == 0.0) {
                newThick = gap / 2;
                numRepIm = 2 * (int) (thick / gap);
                numBlank = 2;
                newTNI = (int) (2 + (2 * thick / gap)) * TNI;
                newZStart = zStart + (sign * ((-thick / 2.f) + (gap / 4.f)));
            } else if ((3 * (gap / thick) % 1) == 0.0) {
                newThick = thick / 3;
                numRepIm = 3;
                numBlank = 3 * (int) (gap / thick);
                newTNI = (int) (3 + (3 * gap / thick)) * TNI;
                newZStart = zStart + (sign * (-thick / 3.f));
            } else if ((3 * (thick / gap) % 1) == 0.0) {
                newThick = gap / 3;
                numRepIm = 3 * (int) (thick / gap);
                numBlank = 3;
                newTNI = (int) (3 + (3 * thick / gap)) * TNI;
                newZStart = zStart + (sign * ((-thick / 2.f) + (gap / 6.f)));
            } else {
                MipavUtil.displayError("Option not implemented for that ratio of thickness to space.");
            }
        } else {
            MipavUtil.displayError("Space between slice centers must be greater than or equal to the slice thickness.");
        }

        return;
    }

    /**
     * Initialize GUI. This is a very simple dialog, with two text fields for user to input information, labels for
     * instructions, labels providing info and an OK and Cancel button.
     */
    private void init() {
        getContentPane().setLayout(new BorderLayout());
        setTitle("Create image volume");
        setForeground(Color.black);

        // set layout
        Box contentBox = new Box(BoxLayout.Y_AXIS);
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        contentBox.addMouseListener(this);

        // define "numbers" and "root" panels and add to content box
        JPanel numbersPanel = new JPanel();
        JPanel rootPanel = new JPanel();
        contentBox.add(numbersPanel);
        contentBox.add(rootPanel);

        // set borders
        numbersPanel.setBorder(buildTitledBorder("Number of Images"));
        numbersPanel.setLayout(gbl);
        rootPanel.setBorder(buildTitledBorder("Root for output files"));

        // place content into layout
        // Total # of images
        JLabel numImagesLabel = new JLabel("Total number of images:");
        numImagesLabel.setFont(serif12);
        numImagesLabel.setForeground(Color.black);
        numImagesLabel.setRequestFocusEnabled(false);
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.gridy = 1;
        numbersPanel.add(numImagesLabel, gbc);

        JLabel numImagesValue = new JLabel(String.valueOf(TNI));
        numImagesValue.setFont(serif12);
        numImagesValue.setForeground(Color.black);
        numImagesValue.setRequestFocusEnabled(false);
        numbersPanel.add(numImagesValue, gbc);

        // Number of planes (entered by user)
        JLabel numPlanesLabel = new JLabel("Number of planes:  ");
        numPlanesLabel.setFont(serif12);
        numPlanesLabel.setForeground(Color.black);
        numPlanesLabel.setRequestFocusEnabled(false);
        gbc.gridy = 2;
        numbersPanel.add(numPlanesLabel, gbc);
        numbersPanel.add(Box.createHorizontalStrut(10));
        numPlanes = new JTextField("1", 4);
        numbersPanel.add(numPlanes, gbc);

        // Number of images per plane
        JLabel numIPPLabel = new JLabel("Images per plane (# of positions):  ");
        numIPPLabel.setFont(serif12);
        numIPPLabel.setForeground(Color.black);
        numIPPLabel.setRequestFocusEnabled(false);
        gbc.gridy = 3;
        numbersPanel.add(numIPPLabel, gbc);
        numIPPValue = new JLabel(String.valueOf(TNI));
        numIPPValue.setFont(serif12);
        numIPPValue.setForeground(Color.black);
        numIPPValue.setRequestFocusEnabled(false);
        numbersPanel.add(numIPPValue, gbc);

        // Root for output files (entered by user)
        JLabel rootLabel = new JLabel("Root for output file names:  ");
        rootLabel.setFont(serif12);
        rootLabel.setForeground(Color.black);
        rootLabel.setRequestFocusEnabled(false);
        rootPanel.add(rootLabel);
        gbc.gridy = 4;
        rootPanel.add(Box.createHorizontalStrut(10));
        root = new JTextField("new_image", 10);

        // root.addActionListener(this);
        rootPanel.add(root);
        buildOKButton();
        buildCancelButton();

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);
        contentBox.add(buttonPanel);
        getContentPane().add(contentBox);
        pack();
        setVisible(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  index  DOCUMENT ME!
     */
    private void saveNewImages(int index) {
        String imDir, imFName;

        // System.out.println("Number of images in position: " +resultImageSet[index].getExtents()[2]);
        imDir = resultImageSet[index].getFileInfo(0).getFileDirectory();

        // System.out.println("Directory for saving " +imDir);
        imFName = resultImageSet[index].getImageName() + ".raw";

        // System.out.println("Filename for saving " +imFName);
        resultImageSet[index].saveImage(imDir, imFName, FileBase.XML, true);
    }

    /**
     * Use the user input to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        Integer tmpInt;
        tmpStr = numPlanes.getText();

        if (testParameter(tmpStr, 1, TNI)) {
            nPlanes = Double.valueOf(tmpStr).intValue();
        } else {
            numPlanes.requestFocus();
            numPlanes.selectAll();

            return false;
        }

        if ((TNI % nPlanes) == 0) {
            numPos = TNI / nPlanes;
            tmpInt = new Integer(numPos);
            numIPPValue.setText(tmpInt.toString());
        } else {
            MipavUtil.displayError("Number of positions must be even divisor of number of images.");
            numPlanes.requestFocus();
            numPlanes.selectAll();

            return false;
        }

        tmpStr = root.getText();
        rootName = String.copyValueOf(tmpStr.toCharArray());

        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  index  DOCUMENT ME!
     */
    private void updateFileInfo(int index) {
        int axisOrient = resultImageSet[index].getFileInfo(0).getAxisOrientation()[2];
        resultImageSet[index].calcMinMax();

        for (int j = 0; j < newNPlanes; j++) {
            resultImageSet[index].getFileInfo(j).setResolutions(newThick, 2);
            resultImageSet[index].getFileInfo(j).setSliceSpacing(newThick);

            if ((axisOrient == 1) || (axisOrient == 4) || (axisOrient == 5)) {
                resultImageSet[index].getFileInfo(j).setOrigin(newZStart + (j * newThick), 2);
            } else {
                resultImageSet[index].getFileInfo(j).setOrigin(newZStart - (j * newThick), 2);
            }
        }
    }
} // end of PlugInDialogCreateVolume class
