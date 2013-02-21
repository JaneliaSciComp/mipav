package gov.nih.mipav.view;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.text.NumberFormat;
import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.tree.*;


/**
 * Tree of images beneath the given directory. When a user clicks on a filename, a thumbnail of the image appears, along
 * with the header data.
 * 
 * @author Neva Cherniavsky
 * @version 1.0 June 1, 2002
 * @see ViewJComponentPreviewImage
 */
public class ViewImageDirectory extends JFrame implements ActionListener, ComponentListener, ItemListener,
        TreeSelectionListener, TreeExpansionListener, ChangeListener, PreviewImageContainer {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1262294439731204344L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected String directory;

    /** DOCUMENT ME! */
    protected JTree directoryTree;

    /** DOCUMENT ME! */
    protected File file;

    /** DOCUMENT ME! */
    protected ViewImageFileFilter imageFilter;

    /** DOCUMENT ME! */
    protected JPanel imagePanel;

    /** DOCUMENT ME! */
    protected ViewJComponentPreviewImage img;

    /** DOCUMENT ME! */
    protected ViewFileTreeNode node;

    /** DOCUMENT ME! */
    protected Font serif12, serif12B;

    /** DOCUMENT ME! */
    protected FileImageXML.Thumbnail thumbnail = null;

    /** DOCUMENT ME! */
    protected JPanel treePanel;

    /** DOCUMENT ME! */
    protected ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private int brightness = 0;

    /** DOCUMENT ME! */
    private JPanel brightPanel;

    /** DOCUMENT ME! */
    private JSlider brightSlider, contSlider;

    /** DOCUMENT ME! */
    private float contrast = 1;

    /** DOCUMENT ME! */
    private JLabel current, current2;

    /** DOCUMENT ME! */
    private Dimension defaultImageSize;

    /** DOCUMENT ME! */
    private JSplitPane imageSliderPane;

    /** DOCUMENT ME! */
    private NumberFormat nfc;

    /** DOCUMENT ME! */
    private boolean openSeparate = false;

    /** DOCUMENT ME! */
    private JCheckBoxMenuItem openSeparateOption;

    /** DOCUMENT ME! */
    private int origBrightness = 0;

    /** DOCUMENT ME! */
    private float origContrast = 1;

    /** DOCUMENT ME! */
    private JLabel otherLabel;

    /** DOCUMENT ME! */
    private ViewTableModel primaryModel;

    /** DOCUMENT ME! */
    private JTable primaryTable;

    /** DOCUMENT ME! */
    private ViewTableModel secondaryModel;

    /** DOCUMENT ME! */
    private JTable secondaryTable;

    /** DOCUMENT ME! */
    private boolean showXMLThumbnail = true;

    /** DOCUMENT ME! */
    private JPanel sliderPanel = null;

    /** DOCUMENT ME! */
    private JCheckBoxMenuItem thumbnailOption;

    /*
     * These next fields that were associated with adding ability to subsample/force UBYTE when opening images and the
     * code in places further down have been commented out becasue we currently do not have the ability to read 3d
     * volumes on a slice by slice basis that would then be subsampled.
     * 
     * we can address this at a later time
     * 
     * nish 6/17/2008
     * 
     */
    /** DOCUMENT ME! */
    // protected JCheckBox enableCheckbox;
    /** DOCUMENT ME! */
    // protected static final String ENABLE = "Enable";
    /** DOCUMENT ME! */
    // protected JTextField txtHeight;
    /** DOCUMENT ME! */
    // protected JTextField txtWidth;
    /** DOCUMENT ME! */
    // protected JCheckBox chkForceUBYTE;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------
    /**
     * Creates new tree of images and sets up file filter. Calls initialization method.
     * 
     * @param dir DOCUMENT ME!
     * @param filter Directory to make tree under.
     */
    public ViewImageDirectory(String dir, ViewImageFileFilter filter) {
        super();
        userInterface = ViewUserInterface.getReference();
        directory = dir;
        this.showXMLThumbnail = Preferences.is(Preferences.PREF_SAVE_XML_THUMBNAIL);

        if (directory.endsWith(":\\")) {
            int index = directory.lastIndexOf('\\');
            String temp = directory.substring(0, index - 1);

            index = temp.lastIndexOf('\\');

            if (index > -1) {
                directory = directory.substring(index + 1);
            }
        }

        imageFilter = filter;
        treePanel = new JPanel(new BorderLayout());
        imagePanel = new JPanel();
        addComponentListener(this);

        JPanel toolPanel = buildToolbar();

        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;
        getContentPane().add(toolPanel, BorderLayout.NORTH);
        init();

        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }

        this.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent we) {

                if (img != null) {
                    img.dispose(true);
                    img = null;
                } else if (thumbnail != null) {
                    thumbnail.finalize();
                    thumbnail = null;
                }

            }
        });

    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Recreates the tree when a new directory is selected; refreshes the tree when refresh is selected.
     * 
     * @param event Event that triggered this function.
     */
    @SuppressWarnings("unchecked")
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("New")) {
            ViewDirectoryChooser chooser = new ViewDirectoryChooser(this);
            String dir = chooser.getImageDirectory();

            if (dir != null) {
                directory = dir;
                treePanel.removeAll();
                buildSourceTreeListing(false);
                validate();
            }
        } /*
             * else if (command.equals(ENABLE)) { txtHeight.setEnabled(enableCheckbox.isSelected());
             * txtWidth.setEnabled(enableCheckbox.isSelected()); chkForceUBYTE.setEnabled(enableCheckbox.isSelected()); }
             */else if (command.equals("Refresh")) {
            treePanel.removeAll();
            buildSourceTreeListing(false);
            validate();
        } else if (command.equals("Filter")) {
            JDialogFilterChoice dialog = new JDialogFilterChoice(this);

            if ( !dialog.isCancelled()) {
                imageFilter = dialog.getFilter();
                treePanel.removeAll();
                buildSourceTreeListing(false);
                validate();
            }
        } else if (command.equals("ToggleThumbnail")) {

            if (thumbnailOption.isSelected()) {
                showXMLThumbnail = true;
            } else {
                showXMLThumbnail = false;
            }
        } else if (command.equals("ToggleOpenSeparate")) {
            openSeparate = openSeparateOption.isSelected();
        } else if (command.equals("Open") || command.equals("OpenToSingle") || command.equals("OpenToSeparate")) {
            int i, j;
            String fileName;
            String newName;
            String directory = ".";
            ModelImage newImage;
            int bufferLength;
            double[] buffer;
            TreePath[] selected = directoryTree.getSelectionPaths();
            FileIO io = new FileIO();
            // Dimension subsampleDimension = null;


            // progressPanel.getProgressBar().setBackground(Color.DARK_GRAY);
            /*
             * if (enableCheckbox.isSelected() && (subsamplingSanityCheck() == false)) {
             * MipavUtil.displayError("Subsampling dimensions are invalid.");
             * 
             * return; }
             * 
             * if (enableCheckbox.isSelected()) { subsampleDimension = new
             * Dimension(Integer.parseInt(txtWidth.getText()), Integer.parseInt(txtHeight.getText())); forceUBYTE =
             * chkForceUBYTE.isSelected(); }
             * 
             */

            if (command.equals("OpenToSingle")) {
                openSeparateOption.setSelected(false);
                openSeparate = false;
            } else if (command.equals("OpenToSeparate")) {
                openSeparateOption.setSelected(true);
                openSeparate = true;
            }

            if (selected == null) {
                MipavUtil.displayError("You must select an image to open.");

                return;
            }
            // if there is only one image selected, open that in a separate
            // frame
            else if ( (selected.length == 1) && ! ((ViewFileTreeNode) selected[0].getLastPathComponent()).isDirectory()) {

                fileName = ((ViewFileTreeNode) selected[0].getLastPathComponent()).getName();
                directory = ((ViewFileTreeNode) selected[0].getLastPathComponent()).getDirectory();

                // io.setPBar(progressPanel);
                io.setQuiet(true);

                ModelImage image = io.readImage(fileName, directory + File.separatorChar);

                if (image == null) {
                    return;
                }

                /*
                 * if(image.getNDims() == 2) { if(subsampleDimension != null) { image = FileIO.subsample(image,
                 * subsampleDimension); } if(forceUBYTE) { if(!image.isColorImage() && image.getType() !=
                 * ModelStorageBase.UBYTE) { image = FileIO.convertToUBYTE(image); } } }
                 */

                new ViewJFrameImage(image, io.getModelLUT());
                // progressPanel.getProgressBar().setBorderPainted(false);
                // progressPanel.getProgressBar().setBackground(this.getBackground());
                // progressPanel.getProgressBar().setForeground(this.getBackground());

                return;
            } else {

                /**
                 * Create a Hashtable to store all selected images
                 */
                Hashtable<String,ModelImage> table = new Hashtable<String,ModelImage>();
                Vector<String> matchingImageNames = new Vector<String>();
                String newDir;
                io.setQuiet(true);
                // io.setPBar(null);

                int progress = 0;
                int add = 100 / selected.length;

                // progressPanel.setValueImmed(progress);

                /**
                 * Add images from FileIO into hashtable
                 */
                for (i = 0; i < selected.length; i++) {
                    newName = ((ViewFileTreeNode) selected[i].getLastPathComponent()).getName();
                    newDir = ((ViewFileTreeNode) selected[i].getLastPathComponent()).getDirectory();

                    // io.setQuiet(true);
                    newImage = io.readImage(newName, newDir + File.separatorChar);
                    table.put(newName, newImage);
                    progress += add;
                    // progressPanel.setValueImmed(progress);
                }

                // progressPanel.setValueImmed(100);

                // progressPanel.getProgressBar().setBorderPainted(false);
                // progressPanel.getProgressBar().setBackground(this.getBackground());
                // progressPanel.getProgressBar().setForeground(this.getBackground());

                ModelImage secondImage = null;

                String[] imageNames = new String[selected.length];

                /**
                 * Continue through the hashtable, getting the image names and storing them in an array, sorting them,
                 * then matching up any images of like dimensions and opening them up into the same frame. the names
                 * must be sorted so that files with sequel number will be placed in the correct order (001, 002, 003
                 * etc). If the open command is told to openSeparate (separately), dimensions will be ignored and each
                 * image will be placed into a separate frame
                 */
                if ( !table.isEmpty()) {
                    Enumeration<String> en = table.keys();

                    int index = 0;

                    while (en.hasMoreElements()) {
                        String nextElement = en.nextElement();

                        if (nextElement != null) {
                            imageNames[index] = nextElement;
                            index++;
                        }
                    }

                    Vector<Vector<String>> extractSubsetsVector = FilenameSorter.extractSubSets(imageNames);
                    Vector<Vector<String>> secondarySortVector = FilenameSorter.secondarySort(extractSubsetsVector);
                    imageNames = FilenameSorter.subSetsToArray(secondarySortVector);

                    newImage = table.get(imageNames[0]);

                    if (newImage != null) {

                        // add the integer key of the first image (image you are
                        // checking against)
                        matchingImageNames.add(imageNames[0]);

                        // go through the rest of the keys in the table
                        for (j = 1; (j < imageNames.length) && (imageNames[j] != null); j++) {
                            secondImage = table.get(imageNames[j]);

                            if (secondImage != null) {

                                if ( (newImage.getType() == secondImage.getType())
                                        && (newImage.getExtents().length == secondImage.getExtents().length)) {
                                    boolean matches = true;

                                    for (i = 0; i < newImage.getExtents().length; i++) {

                                        if ( (newImage.getExtents()[i] != secondImage.getExtents()[i])
                                                || (newImage.getResolutions(0)[i] != secondImage.getResolutions(0)[i])
                                                || (newImage.getExtents().length > 3)) {
                                            matches = false;

                                            break;
                                        }
                                    }

                                    if (matches) {
                                        matchingImageNames.add(imageNames[j]);
                                    }

                                }
                            }

                            secondImage = null; // dont need to clean up since
                            // we have this in table
                        }

                        /**
                         * Find the number of matching images (of like dimensions, resolutions)
                         */
                        int numMatches = matchingImageNames.size();

                        // if are no matching images (numMatches == 1), open it
                        if (numMatches == 1) {
                            table.remove(matchingImageNames.elementAt(0));
                            /*
                             * if(newImage.getNDims() == 2) { if(subsampleDimension != null) { newImage =
                             * FileIO.subsample(newImage, subsampleDimension); } if(forceUBYTE) {
                             * if(!newImage.isColorImage() && newImage.getType() != ModelStorageBase.UBYTE) { newImage =
                             * FileIO.convertToUBYTE(newImage); } } }
                             */
                            // put the image (only 1) into a new frame
                            new ViewJFrameImage(newImage);
                            matchingImageNames.removeAllElements();
                        } else if (openSeparate == true) // open to separate
                        // frames
                        {

                            for (i = 0; i < matchingImageNames.size(); i++) {
                                ModelImage image = table.remove(matchingImageNames.elementAt(i));
                                /*
                                 * if(image.getNDims() == 2) { if(subsampleDimension != null) { image =
                                 * FileIO.subsample(image, subsampleDimension); } if(forceUBYTE) {
                                 * if(!image.isColorImage() && image.getType() != ModelStorageBase.UBYTE) { image =
                                 * FileIO.convertToUBYTE(image); } } }
                                 */
                                new ViewJFrameImage(image);
                            }
                        } else { // otherwise concatenate images into a
                            // single image (2D->3D, 3D->4D)

                            int[] newExtents = new int[newImage.getExtents().length + 1];

                            for (i = 0; i < (newExtents.length - 1); i++) {
                                newExtents[i] = newImage.getExtents()[i];
                            }

                            newExtents[newExtents.length - 1] = matchingImageNames.size();

                            ModelImage concatImage = new ModelImage(newImage.getType(), newExtents, newImage
                                    .getImageName()
                                    + "_concat");

                            if (newImage.isColorImage()) {
                                bufferLength = 4 * newExtents[0] * newExtents[1];
                            } else {
                                bufferLength = newExtents[0] * newExtents[1];
                            }

                            if (newExtents.length > 3) {
                                bufferLength *= newExtents[2];
                            }

                            if (newExtents.length > 4) {
                                bufferLength *= newExtents[3];
                            }

                            buffer = new double[bufferLength];

                            // export the data from each image into the new
                            // image
                            for (i = 0; i < matchingImageNames.size(); i++) {

                                newImage = table.remove(matchingImageNames.elementAt(i));
                                /*
                                 * if(newImage.getNDims() == 2) { if(subsampleDimension != null) { newImage =
                                 * FileIO.subsample(newImage, subsampleDimension); } if(forceUBYTE) {
                                 * if(!newImage.isColorImage() && newImage.getType() != ModelStorageBase.UBYTE) {
                                 * newImage = FileIO.convertToUBYTE(newImage); } }
                                 *  }
                                 */
                                try {
                                    newImage.exportData(0, bufferLength, buffer);
                                } catch (IOException error) {
                                    error.printStackTrace();
                                    MipavUtil.displayError("Error: unable to open image " + i);
                                    Preferences.debug("Error on exportData in ViewImageDirectory");

                                    return;
                                }

                                try {
                                    concatImage.importData(i * bufferLength, buffer, false);
                                } catch (IOException error) {
                                    error.printStackTrace();
                                    MipavUtil.displayError("IO error on import into concat images");
                                    Preferences.debug("Error on importData in ViewImageDirectory");

                                    return;
                                }

                                // once again, CLEAN UP THE IMAGE!
                                newImage.disposeLocal();
                            }

                            matchingImageNames.removeAllElements();

                            concatImage.calcMinMax();

                            new ViewJFrameImage(concatImage);

                        }

                    }

                    // clear out the sorted names
                    for (j = 0; j < imageNames.length; j++) {
                        imageNames[j] = null;
                    }
                }
            }
        }

    }

    /**
     * Unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void componentHidden(ComponentEvent event) {}

    /**
     * Unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void componentMoved(ComponentEvent event) {}

    /**
     * Tells the component image that the size of the image panel has changed, then repaints the component image.
     * 
     * @param event Event that triggered this function.
     */
    public void componentResized(ComponentEvent event) {

        if (event.getSource().equals(imagePanel)) {

            if (img != null) {

                if (imagePanel != null) {
                    img.setImgSize(imagePanel.getBounds().width, imagePanel.getBounds().height);
                } else {
                    img.setImgSize(400, 200);
                }

                img.paintComponent(img.getGraphics());
            }
        } else if (event.getSource().equals(sliderPanel)) {}
    }

    /**
     * Unchanged.
     * 
     * @param event DOCUMENT ME!
     */
    public void componentShown(ComponentEvent event) {}

    /**
     * Called by the component image to get the real-time size of the panel before centering.
     * 
     * @return The size of the panel.
     */
    public Dimension getPanelSize() {
        return new Dimension(imagePanel.getBounds().width, imagePanel.getBounds().height);
    }

    /**
     * Gets the ViewJComponentPreviewImage that contains the currently selected model image.
     * 
     * @return Gets the contained preview image component.
     */
    public ViewJComponentPreviewImage getComponentPreviewImage() {
        return img;
    }

    /**
     * Sets border painted or not painted depending on if the button was selected or deselected.
     * 
     * @param event Event that triggered this function.
     */
    public void itemStateChanged(ItemEvent event) {
        ((AbstractButton) event.getSource()).setBorderPainted(event.getStateChange() == ItemEvent.SELECTED);
    }

    /**
     * Sets values based on knob along slider.
     * 
     * @param e Event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == brightSlider) {
            brightness = brightSlider.getValue();
            current.setText(String.valueOf(brightness));

            // if the image is not a thumbnail and it is larger than 1024 x 768,
            // do not adjust while sliding
            if ( (img != null) && (img.getImageSize() > 786432) && brightSlider.getValueIsAdjusting()) {
                return;
            }

            // Change only the brightness and contrast of the current slice
            if (img != null) {
                img.setSliceBrightness(brightness, contrast);
            } else if (thumbnail != null) {
                thumbnail.setBrightnessContrast(brightness, contrast);
            }
        } else if (source == contSlider) {
            contrast = (float) Math.pow(10.0, contSlider.getValue() / 200.0);
            current2.setText(String.valueOf(nfc.format(contrast)));

            // if the image is not a thumbnail and it is larger than 1024 x 768,
            // do not adjust while sliding
            if ( (img != null) && (img.getImageSize() > 786432) && contSlider.getValueIsAdjusting()) {
                return;
            }

            // Change only the brightness and contrast of the current slice
            if (img != null) {
                img.setSliceBrightness(brightness, contrast);
            } else if (thumbnail != null) {
                thumbnail.setBrightnessContrast(brightness, contrast);
            }

        }
    }

    /**
     * Unchanged.
     * 
     * @param tee DOCUMENT ME!
     */
    public void treeCollapsed(TreeExpansionEvent tee) {}

    /**
     * Expands tree node in file tree. On expansion, the tree queries the selected node; if a selected node has had its
     * children previously added, then no nodes will be added, but the tree will display the previously added children.
     * Otherwize, the node will add nodes which will be displayed; each node will be marked as adding only directories
     * as child-nodes.
     * 
     * @param tee Event that triggered this function.
     */
    public void treeExpanded(TreeExpansionEvent tee) {
        TreePath path = tee.getPath();

        node = (ViewFileTreeNode) path.getLastPathComponent();
        file = (File) node.getUserObject();

        if ( !node.isExplored()) {
            DefaultTreeModel model = (DefaultTreeModel) directoryTree.getModel();

            node.explore(imageFilter);
            model.nodeStructureChanged(node);
        }
    }

    /**
     * Re-create the image and header info by reacting to each selection on the tree. On selection, the image is read in
     * again and constructed, and the table is cleared and the new header info is posted to the table. Note: no check
     * has been made to remove child-nodes from a selection when the parent has been selected.
     * 
     * @param e Event that triggered this function.
     */
    public void valueChanged(TreeSelectionEvent e) {
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) directoryTree.getLastSelectedPathComponent();

        if (node == null) {
            return;
        }

        if ( ! ((ViewFileTreeNode) node).isDirectory()) {
            FileInfoBase fileInfo = buildImage( ((ViewFileTreeNode) node).getName(), ((ViewFileTreeNode) node)
                    .getDirectory()
                    + File.separatorChar);

            if (fileInfo == null) {
                return;
            }

            primaryModel.setRowCount(0);
            secondaryModel.setRowCount(0);
            otherLabel.setVisible(false);

            String[] info = new String[] {"", ""};

            for (int i = 0; i < fileInfo.getExtents().length; i++) {
                info[0] = "Dimension " + i;
                info[1] = Integer.toString(fileInfo.getExtents()[i]);
                primaryModel.addRow(info);
            }

            info[0] = "Type";
            info[1] = ModelStorageBase.getBufferTypeStr(fileInfo.getDataType());
            primaryModel.addRow(info);

            info[0] = "Max";
            info[1] = Double.toString(fileInfo.getMax());
            primaryModel.addRow(info);

            info[0] = "Min";
            info[1] = Double.toString(fileInfo.getMin());
            primaryModel.addRow(info);

            info[0] = "Modality";
            info[1] = FileInfoBase.getModalityStr(fileInfo.getModality());
            primaryModel.addRow(info);

            info[0] = "Orientation";
            info[1] = FileInfoBase.getImageOrientationStr(fileInfo.getImageOrientation());
            primaryModel.addRow(info);

            float[] resolutions;

            resolutions = fileInfo.getResolutions();

            int[] measure;

            measure = fileInfo.getUnitsOfMeasure();

            for (int i = 0; i < fileInfo.getExtents().length; i++) {

                if (resolutions[i] > 0.0) {
                    info[0] = "Pixel resolution " + i;
                    info[1] = Float.toString(resolutions[i]) + " " + (Unit.getUnitFromLegacyNum(measure[i])).toString();
                    primaryModel.addRow(info);
                } // end of if (resolutions[i] > 0.0)
            } // for (i=0; i < 5; i++)

            info[0] = "Endianess";

            if (fileInfo.getEndianess() == FileBase.LITTLE_ENDIAN) {
                info[1] = "Little Endian";
            } else {
                info[1] = "Big Endian";
            }

            if (fileInfo.getFileFormat() == FileUtility.DICOM) {
                otherLabel.setVisible(true);
                JDialogFileInfoDICOM.showTags(secondaryModel, (FileInfoDicom) fileInfo, false);
            }
        }

        if (img != null) {
            img.setSliceBrightness(brightness, contrast);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param fileName String
     * @param directory String
     * 
     * @return FileInfoBase
     */
    protected FileInfoBase buildImage(String fileName, String directory) {

        FileIO io = new FileIO();
        FileInfoBase fileInfo = null;

        if (img != null) {
            imagePanel.remove(img);
            img.dispose(false);
            img = null;
        } else if (thumbnail != null) {
            imagePanel.remove(thumbnail);
            thumbnail.finalize();
        }

        if (showXMLThumbnail && (fileName.endsWith(".xml") || fileName.endsWith(".XML"))) {
            FileImageXML xmlTemp = io.readXMLThumbnail(fileName, directory);

            if (xmlTemp != null) {
                fileInfo = xmlTemp.getFileInfo();
                thumbnail = xmlTemp.getThumbnail();
                imagePanel.add(thumbnail, BorderLayout.CENTER);
                imagePanel.validate();
                imagePanel.repaint();
                thumbnail.setBrightnessContrast(brightness, contrast);

                return fileInfo;
            }
        }

        // this is used to pass in our progress bar panel to the file IO to tell
        // it to use
        // if (progressPanel != null) {
        // progressPanel.getProgressBar().setBackground(Color.DARK_GRAY);
        // io.setPBar(this.progressPanel);
        // }

        io.setQuiet(true);

        ModelImage image = io.readOneImage(fileName, directory);

        if (image == null) {
            return null;
        }

        // if (progressPanel != null) {
        // progressPanel.getProgressBar().setBorderPainted(false);
        // progressPanel.getProgressBar().setBackground(this.getBackground());
        // progressPanel.getProgressBar().setForeground(this.getBackground());
        // }

        int[] extents = new int[] {image.getExtents()[0], image.getExtents()[1]};

        img = new ViewJComponentPreviewImage(image, extents, this);
        img.createImg(0);
        imagePanel.add(img);

        imagePanel.validate();
        imagePanel.repaint();

        fileInfo = (FileInfoBase) image.getFileInfo(0).clone();

        return fileInfo;
    }

    /**
     * Creates the tree that holds the image files and returns the panel containing the tree.
     * 
     * @param directoriesOnly DOCUMENT ME!
     */
    protected void buildSourceTreeListing(boolean directoriesOnly) {

        // build a directory tree by first finding the roots of the filesystem.
        DefaultMutableTreeNode fs = new DefaultMutableTreeNode("Computer");
        ViewFileTreeNode rootNode = null;

        if (fs != null) { // fs is null when the set of roots could not be
            // determined
            rootNode = new ViewFileTreeNode(new File(directory), true);
            rootNode.exploreDirectoriesOnly(directoriesOnly);
            fs.add(rootNode);
            rootNode.getPath();
            directoryTree = new JTree(fs);
            directoryTree.setRootVisible(false);
        } else { // we can build an empty tree, but it won't mean anything.
            // throw error?? FIXME
            directoryTree = new JTree();
        }

        JScrollPane treeScroll = new JScrollPane(directoryTree);

        treePanel.add(treeScroll, BorderLayout.CENTER);
        treePanel.setPreferredSize(new Dimension(200, 450));
        directoryTree.getSelectionModel().setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
        directoryTree.addTreeSelectionListener(this);
        directoryTree.addTreeExpansionListener(this);

        directoryTree.expandRow(0);
    }

    /**
     * Builds a titled border with the given title, an etched border, and the proper font and color.
     * 
     * @param title Title of the border
     * 
     * @return The titled border.
     */
    protected TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                Color.black);
    }

    /**
     * Builds a toolbar with the same functionality as the menu.
     * 
     * @return DOCUMENT ME!
     */
    protected JPanel buildToolbar() {
        Border pressedBorder = BorderFactory.createLoweredBevelBorder();
        Border etchedBorder = BorderFactory.createEtchedBorder();

        JPanel panel = new JPanel(new BorderLayout());
        JToolBar tBar = new JToolBar();

        tBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        tBar.setBorder(etchedBorder);
        tBar.setBorderPainted(true);

        JButton openButton = new JButton(MipavUtil.getIcon("openintosingle.gif"));

        openButton.addActionListener(this);
        openButton.setToolTipText("Open selected images into single frame");
        openButton.setActionCommand("OpenToSingle");
        openButton.setBorderPainted(false);
        openButton.setRolloverEnabled(true);
        openButton.setRolloverIcon(MipavUtil.getIcon("openintosingleroll.gif"));
        openButton.setBorder(pressedBorder);
        openButton.addItemListener(this);
        openButton.setFocusPainted(false);
        tBar.add(openButton);

        JButton openToSeparateButton = new JButton(MipavUtil.getIcon("openintoseparate.gif"));
        openToSeparateButton.addActionListener(this);
        openToSeparateButton.setToolTipText("Open selected images into separate frames");
        openToSeparateButton.setActionCommand("OpenToSeparate");
        openToSeparateButton.setBorderPainted(false);
        openToSeparateButton.setRolloverEnabled(true);
        openToSeparateButton.setRolloverIcon(MipavUtil.getIcon("openintoseparateroll.gif"));
        openToSeparateButton.setBorder(pressedBorder);
        openToSeparateButton.addItemListener(this);
        openToSeparateButton.setFocusPainted(false);
        tBar.add(openToSeparateButton);

        JButton refreshButton = new JButton(MipavUtil.getIcon("refresh.gif"));

        refreshButton.addActionListener(this);
        refreshButton.setToolTipText("Refresh file list");
        refreshButton.setActionCommand("Refresh");
        refreshButton.setBorderPainted(false);
        refreshButton.setRolloverEnabled(true);
        refreshButton.setRolloverIcon(MipavUtil.getIcon("refreshroll.gif"));
        refreshButton.setBorder(pressedBorder);
        refreshButton.addItemListener(this);
        refreshButton.setFocusPainted(false);
        tBar.add(refreshButton);

        JButton newButton = new JButton(MipavUtil.getIcon("new.gif"));

        newButton.addActionListener(this);
        newButton.setToolTipText("New top directory");
        newButton.setActionCommand("New");
        newButton.setBorderPainted(false);
        newButton.setRolloverEnabled(true);
        newButton.setRolloverIcon(MipavUtil.getIcon("newroll.gif"));
        newButton.setBorder(pressedBorder);
        newButton.addItemListener(this);
        newButton.setFocusPainted(false);
        tBar.add(newButton);

        JButton filterButton = new JButton(MipavUtil.getIcon("filter.gif"));

        filterButton.addActionListener(this);
        filterButton.setToolTipText("Reset filter");
        filterButton.setActionCommand("Filter");
        filterButton.setBorderPainted(false);
        filterButton.setRolloverEnabled(true);
        filterButton.setRolloverIcon(MipavUtil.getIcon("filterroll.gif"));
        filterButton.setBorder(pressedBorder);
        filterButton.addItemListener(this);
        filterButton.setFocusPainted(false);
        tBar.add(filterButton);

        tBar.setFloatable(false);
        panel.add(tBar, BorderLayout.NORTH);

        return panel;
    }

    /**
     * Initializes the GUI components and adds them to the main frame.
     */
    protected void init() {
        setTitle("Tree of images in selected directory");

        buildMenu();
        buildSourceTreeListing(false);
        buildBrightContPanel();

        // This is very strange - a JSplit.HORIZONTAL_SPLIT in the
        // constructor is used to create a vertical split in the
        // output.
        JSplitPane mainPanel = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true, treePanel, buildImagePanel());

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        pack();
        setLocation(200, 200);
        setVisible(true);

    }

    /**
     * Initializes GUI components and displays dialog.
     * 
     * <p>
     * For the brightSlider the slider values and the brightness values are identical. brightness is an offset going
     * from -255 to 255. This is enough to change all 0 values to 255 and all 255 values to 0. brightness is added to
     * all contrast scaled red, green, and blue.
     * </p>
     * 
     * <p>
     * However, for the contrastSlider the slider values are different from the contrast values. The contrast values go
     * from 0.1 to 10.0 while the slider values go from -200 to 200. contrast =
     * (float)Math.pow(10.0,contSlider.getValue()/200.0) The original red, green, and blue are mutliplied by contrast.
     * </p>
     */
    private void buildBrightContPanel() {
        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;
        brightSlider = new JSlider(JSlider.HORIZONTAL, -255, 255, origBrightness);

        brightSlider.setMajorTickSpacing(102);
        brightSlider.setPaintTicks(true);
        brightSlider.setEnabled(true);
        brightSlider.addChangeListener(this);

        JLabel maximum = new JLabel(String.valueOf(255));

        maximum.setForeground(Color.black);
        maximum.setFont(serif12);

        current = new JLabel(String.valueOf(origBrightness));
        current.setForeground(Color.black);
        current.setFont(serif12B);

        JLabel minimum = new JLabel(String.valueOf( -255));

        minimum.setForeground(Color.black);
        minimum.setFont(serif12);

        sliderPanel = new JPanel(new GridBagLayout());
        sliderPanel.addComponentListener(this);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(brightSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel.add(minimum, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel.add(current, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel.add(maximum, gbc);
        sliderPanel.setBorder(buildTitledBorder("Level"));

        contSlider = new JSlider(JSlider.HORIZONTAL, -200, 200,
                (int) (Math.round(86.85889638 * Math.log(origContrast))));

        contSlider.setMajorTickSpacing(80);
        contSlider.setPaintTicks(true);
        contSlider.setEnabled(true);
        contSlider.addChangeListener(this);

        JLabel maximum2 = new JLabel(String.valueOf(10));

        maximum2.setForeground(Color.black);
        maximum2.setFont(serif12);

        nfc = NumberFormat.getNumberInstance();
        nfc.setMaximumFractionDigits(3);

        current2 = new JLabel(String.valueOf(nfc.format(origContrast)));
        current2.setForeground(Color.black);
        current2.setFont(serif12B);

        JLabel minimum2 = new JLabel(String.valueOf(0.100));

        minimum2.setForeground(Color.black);
        minimum2.setFont(serif12);

        JPanel sliderPanel2 = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel2.add(contSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel2.add(minimum2, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel2.add(current2, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel2.add(maximum2, gbc);
        sliderPanel2.setBorder(buildTitledBorder("Window"));

        JPanel centerPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc2 = new GridBagConstraints();

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 1;
        gbc2.gridheight = 2;
        centerPanel.add(sliderPanel2, gbc2);

        gbc2.gridy = 2;
        centerPanel.add(sliderPanel, gbc2);

        // gbc2.gridy = 4;
        // centerPanel.add(buildSubsamplePanel(), gbc2);

        gbc2.gridheight = 1;
        gbc2.gridy = 4;

        // progressPanel = new JPanelProgressBar(0, 100);
        // centerPanel.add(progressPanel, gbc2);

        // progressPanel.getProgressBar().setBackground(this.getBackground());
        // progressPanel.getProgressBar().setForeground(this.getBackground());
        // progressPanel.getProgressBar().setBorderPainted(false);

        brightPanel = new JPanel(new BorderLayout());
        brightPanel.add(centerPanel);
        brightPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        brightPanel.setPreferredSize(new Dimension(190, 100));

        brightPanel.setMinimumSize(new Dimension(190, 100));

    }

    /**
     * Sets up the image panel and the table that will store basic header info, and returns the panel containing these.
     * 
     * @return The panel containing the image and the header info table.
     */
    private JSplitPane buildImagePanel() {

        // JPanel panel = new JPanel(new GridBagLayout());
        JPanel tablePanel = new JPanel(new BorderLayout());

        defaultImageSize = new Dimension(300, 190);
        imagePanel.setPreferredSize(defaultImageSize);

        Box scrollingBox;
        JScrollPane scrollPane;

        try {
            scrollingBox = new Box(BoxLayout.Y_AXIS);

            primaryModel = new ViewTableModel();
            primaryTable = new JTable(primaryModel);
            secondaryModel = new ViewTableModel();
            secondaryTable = new JTable(secondaryModel);
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("ViewFileInfo reports: Out of memory!");

            return null;
        }

        primaryModel.addColumn("Name");
        primaryModel.addColumn("Value");

        primaryTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        primaryTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        primaryTable.getColumn("Name").setMinWidth(160);
        primaryTable.getColumn("Name").setMaxWidth(500);
        primaryTable.getColumn("Value").setMinWidth(50);
        primaryTable.getColumn("Value").setMaxWidth(1000);

        JLabel headerLabel = new JLabel("Image Information");

        headerLabel.setForeground(Color.black);
        headerLabel.setFont(MipavUtil.font12);
        headerLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        scrollingBox.add(headerLabel);
        scrollingBox.add(primaryTable);

        secondaryModel.addColumn("Selected");
        secondaryModel.addColumn("Tag");
        secondaryModel.addColumn("Name");
        secondaryModel.addColumn("Value");

        secondaryTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        secondaryTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

        secondaryTable.getColumn("Selected").setMinWidth(90);
        secondaryTable.getColumn("Selected").setMaxWidth(90);
        secondaryTable.getColumn("Tag").setMinWidth(90);
        secondaryTable.getColumn("Tag").setMaxWidth(90);
        secondaryTable.getColumn("Name").setMinWidth(160);
        secondaryTable.getColumn("Name").setMaxWidth(500);
        secondaryTable.getColumn("Value").setMinWidth(50);
        secondaryTable.getColumn("Value").setMaxWidth(1000);

        otherLabel = new JLabel("Other Information");
        otherLabel.setForeground(Color.black);
        otherLabel.setFont(MipavUtil.font12);
        otherLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        otherLabel.setVisible(false);
        scrollingBox.add(otherLabel);
        scrollingBox.add(secondaryTable);

        try {
            scrollPane = new JScrollPane(scrollingBox, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                    JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane.setPreferredSize(new Dimension(400, 200));
            scrollPane.setMinimumSize(new Dimension(150, 100));
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("ViewFileInfo reports: Out of memory!");

            return null;
        }

        scrollPane.setBackground(Color.black);

        tablePanel.add(scrollPane);
        tablePanel.setPreferredSize(new Dimension(400, 180));

        // A HORIZONTAL_SPLIT would be expected here
        // JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
        // true, imagePanel, tablePanel);
        imageSliderPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true, imagePanel, brightPanel);
        imageSliderPane.setResizeWeight(.9);

        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, imageSliderPane, tablePanel);

        splitPane.setResizeWeight(.9);

        return splitPane;
    }

    /**
     * Builds a small menu with "New directory", "Refresh directory", "Reset file filter", and "Open image" options.
     */
    private void buildMenu() {
        JMenu menu;
        JMenuBar menuBar;
        JMenuItem itemRefresh;
        JMenuItem itemNew;
        JMenuItem itemFilter;
        JMenuItem itemOpen;

        menuBar = new JMenuBar();
        menu = new JMenu("File");
        itemNew = new JMenuItem("New directory");
        itemRefresh = new JMenuItem("Refresh list");
        itemFilter = new JMenuItem("Reset filter");
        itemOpen = new JMenuItem("Open selected images");

        openSeparateOption = new JCheckBoxMenuItem("Open each image separately", false);
        openSeparateOption.setFont(MipavUtil.font12B);
        openSeparateOption.addActionListener(this);
        openSeparateOption.setActionCommand("ToggleOpenSeparate");

        thumbnailOption = new JCheckBoxMenuItem("Show XML thumbnail if available");
        thumbnailOption.setFont(MipavUtil.font12B);
        thumbnailOption.setSelected(showXMLThumbnail);
        thumbnailOption.addActionListener(this);
        thumbnailOption.setActionCommand("ToggleThumbnail");

        menu.setFont(MipavUtil.font12B);
        itemNew.setFont(MipavUtil.font12B);
        itemNew.setActionCommand("New");
        itemNew.addActionListener(this);
        itemRefresh.setFont(MipavUtil.font12B);
        itemRefresh.setActionCommand("Refresh");
        itemRefresh.addActionListener(this);
        itemFilter.setFont(MipavUtil.font12B);
        itemFilter.setActionCommand("Filter");
        itemFilter.addActionListener(this);
        itemOpen.setFont(MipavUtil.font12B);
        itemOpen.setActionCommand("Open");
        itemOpen.addActionListener(this);

        menu.add(itemNew);
        menu.add(itemRefresh);
        menu.add(itemFilter);
        menu.add(new JSeparator());
        menu.add(itemOpen);
        menu.add(openSeparateOption);
        menu.add(thumbnailOption);
        menuBar.add(menu);
        setJMenuBar(menuBar);
    }

    /*
     * protected JPanel buildSubsamplePanel() { GridBagLayout gbLayout = new GridBagLayout(); GridBagConstraints
     * gbConstraints = new GridBagConstraints();
     * 
     * JPanel subsamplePanel = new JPanel(gbLayout); subsamplePanel.setBorder(new
     * TitledBorder(BorderFactory.createEtchedBorder(), "Subsampling"));
     * 
     * gbConstraints.gridx = 1; gbConstraints.anchor = GridBagConstraints.WEST; enableCheckbox = new JCheckBox(ENABLE);
     * enableCheckbox.addActionListener(this); enableCheckbox.setActionCommand(ENABLE);
     * gbLayout.setConstraints(enableCheckbox, gbConstraints); subsamplePanel.add(enableCheckbox);
     * 
     * gbConstraints.gridx = 0; gbConstraints.gridy = 1; gbConstraints.anchor = GridBagConstraints.EAST;
     * 
     * JLabel lblWidth = new JLabel("Width: "); gbLayout.setConstraints(lblWidth, gbConstraints);
     * subsamplePanel.add(lblWidth);
     * 
     * gbConstraints.gridx = 1; gbConstraints.anchor = GridBagConstraints.WEST; txtWidth = new JTextField(5);
     * txtWidth.setEnabled(enableCheckbox.isSelected()); gbLayout.setConstraints(txtWidth, gbConstraints);
     * subsamplePanel.add(txtWidth);
     * 
     * gbConstraints.gridx = 0; gbConstraints.gridy = 2; gbConstraints.anchor = GridBagConstraints.EAST;
     * 
     * JLabel lblHeight = new JLabel("Height: "); gbLayout.setConstraints(lblHeight, gbConstraints);
     * subsamplePanel.add(lblHeight);
     * 
     * gbConstraints.gridx = 1; gbConstraints.anchor = GridBagConstraints.WEST; txtHeight = new JTextField(5);
     * txtHeight.setEnabled(enableCheckbox.isSelected()); gbLayout.setConstraints(txtHeight, gbConstraints);
     * subsamplePanel.add(txtHeight);
     * 
     * gbConstraints.gridy++; chkForceUBYTE = new JCheckBox("Force 8-bit"); chkForceUBYTE.setToolTipText("Force result
     * images to use 8-bit channels"); chkForceUBYTE.setEnabled(enableCheckbox.isSelected());
     * gbLayout.setConstraints(chkForceUBYTE, gbConstraints); subsamplePanel.add(chkForceUBYTE);
     * 
     * return subsamplePanel; }
     * 
     * 
     * 
     * 
     * protected boolean subsamplingSanityCheck() {
     * 
     * try { int height = Integer.parseInt(txtHeight.getText()); int width = Integer.parseInt(txtWidth.getText());
     * 
     * if ((height < 1) || (width < 1)) { return false; }
     * 
     * return true; } catch (NumberFormatException nfe) { return false; } }
     */

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------
    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private class ImageNameComparator implements Comparator<String> {

        /**
         * DOCUMENT ME!
         * 
         * @param nameA DOCUMENT ME!
         * @param nameB DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(String nameA, String nameB) {

            if ( (nameA == null) && (nameB == null)) {
                return 0;
            } else if (nameA == null) {
                return -1;
            } else if (nameB == null) {
                return 0;
            }

            String a = nameA.toLowerCase();
            String b = nameB.toLowerCase();

            int a_int;
            int b_int;

            if ( ! (FileUtility.trimNumbersAndSpecial(a).equals(FileUtility.trimNumbersAndSpecial(b)))) {
                return a.compareTo(b);
            } else {
                a_int = FileUtility.getFileIndex(a);
                b_int = FileUtility.getFileIndex(b);
            }

            if (a_int > b_int) {
                return 1;
            } else if (b_int > a_int) {
                return -1;
            }

            return 0;
        }
    }

}
