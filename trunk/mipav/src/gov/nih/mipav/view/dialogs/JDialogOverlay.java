package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * <p>Title: JDialogOverlay</p>
 *
 * <p>Description: dialog for choosing 16 overlays (dicom tags or image attributes)</p>
 *
 * <p>Copyright: Copyright (c) 2003</p>
 *
 * <p>Company:</p>
 *
 * @author    benny the link
 * @stardate  1.0adf152345-alpha-gamma
 */
public class JDialogOverlay extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2470981064995748425L;

    /** blank overlay string. */
    public static String BLANK_OVERLAY = " [Blank Overlay] ";

    /** attribute string (for image attributes). */
    public static String[] attribStr = {
    	"Blank Overlay","Dimension 0", "Dimension 1", "Dimension 2", "Dimension 3", "Type", "Min", "Max", "Orientation",
        "Axis X Orientation", "Axis Y Orientation", "Axis Z Orientation", "Pixel resolution 0", "Pixel resolution 1",
        "Pixel resolution 2", "Pixel resolution 3", "Slice thickness", "Origin 0", "Origin 1", "Origin 2", "Origin 3",
        "Endianess"
    };

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String[] borderTitles = {
        "Upper left corner", "Upper right corner", "Lower left corner", "Lower right corner"
    };

    /** DOCUMENT ME! */
    private JButton[] buttonArray = new JButton[16];

    /** DOCUMENT ME! */
    private String headerString = null;

    /** DOCUMENT ME! */
    private boolean isDicom = true;

    /** DOCUMENT ME! */
    private JTextField[] nameArray = new JTextField[16];

    /** DOCUMENT ME! */
    private String[] overlayNames = new String[16];

    /** DOCUMENT ME! */
    private String[] overlayValues = new String[16];

    /** DOCUMENT ME! */
    private JDialogChooseOverlay tagDialog;
    
    private JCheckBox showOverlayBox;
    
    private ViewJComponentEditImage comp;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * constructor to be called from ViewJFrameImage.
     *
     * @param  theParentFrame  Frame the parent frame
     * @param  isDicom         boolean is the image dicom
     * @param  headerStr       DOCUMENT ME!
     */
    public JDialogOverlay(Frame theParentFrame, boolean isDicom, String headerStr,ViewJComponentEditImage comp) {
        super(theParentFrame, false);
        this.comp = comp;
        this.isDicom = isDicom;
        this.headerString = headerStr;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * performed an action!
     *
     * @param  e  ActionEvent
     */
    public void actionPerformed(ActionEvent e) {

        if (e.getSource().equals(OKButton)) {
            String val;

            for (int i = 0; i < 16; i++) {
                val = buttonArray[i].getText();

                if (val.equals(BLANK_OVERLAY) || val.equals("Blank Overlay")) {
                    overlayValues[i] = new String("-");
                } else if (!isDicom) {
                    overlayValues[i] = new String(val);
                } else {
                    overlayValues[i] = new String(val.substring(1, val.length() - 1));
                }
            }

            for (int i = 0; i < 16; i++) {
                overlayNames[i] = new String(nameArray[i].getText());
            }

            Preferences.setOverlays(isDicom, overlayValues);

            Preferences.setOverlayNames(isDicom, overlayNames);

            if(showOverlayBox.isSelected()) {
      		  comp.setOverlay(true);
      		  if(isDicom) {
      			Preferences.setShowDICOMOverlays(true);
      		  }else {
      			  Preferences.setShowImageOverlays(true);
      		  }
      	  	}else {
      		  comp.setOverlay(false);
      		  if(isDicom) {
      			Preferences.setShowDICOMOverlays(false);
      		  }else {
      			  Preferences.setShowImageOverlays(false);
      		  }
      	  	}
            
            comp.paintComponent(comp.getGraphics());
            

            if (parentFrame instanceof ViewJFrameImage) {
                ((ViewJFrameImage) parentFrame).updateImages();
            }

            //setVisible(false);
           // dispose();
        } else if (e.getSource().equals(cancelButton)) {
            setVisible(false);
            dispose();
        } else {

            for (int i = 0; i < 16; i++) {

                if (e.getSource().equals(buttonArray[i])) {

                    if (headerString != null) {
                        overlayValues[i] = new String(headerString);
                        buttonArray[i].setText("(" + overlayValues[i] + ")");
                        headerString = null;

                        if (isDicom) {
                            FileDicomKey tagKey = new FileDicomKey(overlayValues[i]);

                            FileDicomTagInfo info = DicomDictionary.getInfo(tagKey);
                            nameArray[i].setText(info.getName());
                        }
                    } else {
                        tagDialog.setButton(buttonArray[i]);
                        tagDialog.setName(nameArray[i]);
                        tagDialog.setVisible(true);
                    }

                    break;
                }
            }
        }
    }

    /**
     * cleanup on closing.
     *
     * @param  event  WindowEvent
     */
    public void windowClosing(WindowEvent event) {

        if (tagDialog != null) {
            tagDialog.dispose();
        }

        dispose();
    }

    /**
     * builds each quadrant panel that has 4 buttons.
     *
     * @param   quadrant  int (0-4)
     *
     * @return  JPanel
     */
    private JPanel buildPanel(int quadrant) {
        JPanel panel = new JPanel();

        panel.setBorder(buildTitledBorder(borderTitles[quadrant]));

        GridBagConstraints gbc = new GridBagConstraints();
        panel.setLayout(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(10, 10, 10, 10);

        // create the buttons and add them to the panel
        int start = quadrant * 4;
        int end = start + 4;

        for (int i = start; i < end; i++) {
            gbc.gridx = 0;
            gbc.fill = GridBagConstraints.VERTICAL;
            gbc.weightx = 0;
            gbc.weighty = 1;
            buttonArray[i] = new JButton();
            buttonArray[i].setFont(MipavUtil.font12);
            buttonArray[i].addActionListener(this);
            buttonArray[i].setHorizontalAlignment(JButton.LEFT);
            panel.add(buttonArray[i], gbc);

            gbc.weightx = 1;
            gbc.fill = GridBagConstraints.BOTH;
            nameArray[i] = new JTextField(7);
            nameArray[i].setEditable(false);
            nameArray[i].setFont(MipavUtil.font12);

            gbc.gridx++;
            panel.add(nameArray[i], gbc);

            gbc.gridy++;
        }

        return panel;
    }

    /**
     * initialize the dialog.
     */
    private void init() {


        String title = "Overlay Options";

        if (isDicom) {
            title = "DICOM " + title;
        }

        tagDialog = new JDialogChooseOverlay(parentFrame);

        setTitle(title);

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new GridLayout(2, 2));

        for (int i = 0; i < 4; i++) {
            mainPanel.add(buildPanel(i));
        }

        setOverlayButtons();

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        OKButton.setText("Apply");
        buildCancelButton();
        buttonPanel.add(cancelButton);
        cancelButton.setText("Close");
        
        showOverlayBox = new JCheckBox("Show overlay");
        showOverlayBox.setSelected(true);

        this.getContentPane().add(mainPanel, BorderLayout.NORTH);
        this.getContentPane().add(showOverlayBox, BorderLayout.CENTER);
        this.getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        //setSize(670, 500);
        setResizable(false);
        this.setMinimumSize(new Dimension(670,(int)this.getSize().getHeight()));
        setVisible(true);
    }

    /**
     * Sets the text for the overlay buttons (dicom or image attrib).
     */
    private void setOverlayButtons() {

        overlayValues = Preferences.getOverlays(isDicom);

        overlayNames = Preferences.getOverlayNames(isDicom);

        for (int i = 0; i < 16; i++) {

            if ((overlayValues[i] == null) || overlayValues[i].equals("-")) {
                buttonArray[i].setText(BLANK_OVERLAY);

            } else {

                if (isDicom) {
                    buttonArray[i].setText("(" + overlayValues[i] + ")");
                    nameArray[i].setText(overlayNames[i]);
                    // FileDicomKey tagKey = new FileDicomKey(overlayValues[i]);

                    // FileDicomTag tag = (FileDicomTag) (dicomTable.get(tagKey));
                    // nameArray[i].setText(tag.getName());

                } else {
                    buttonArray[i].setText(overlayValues[i]);
                    nameArray[i].setText(overlayNames[i]);
                }
            }
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * <p>Title: JDialogDICOMTags</p>
     *
     * <p>Description: Class for selecting any dicom tag from the dicom dictionary (if dicom) or image attributes (if
     * not dicom)</p>
     *
     * <p>Copyright: Copyright (c) 2003</p>
     *
     * <p>Company:</p>
     *
     * @author   not attributable
     * @version  1.0
     */
    private class JDialogChooseOverlay extends JDialogBase {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 2690526125316423716L;

        /** DOCUMENT ME! */
        private JButton currentButton;

        /** DOCUMENT ME! */
        private JTextField currentName;

        /** DOCUMENT ME! */
        private ViewTableModel model;

        /** DOCUMENT ME! */
        private JScrollPane scrollPane;

        /** DOCUMENT ME! */
        private JTable table;

        /**
         * Creates a new JDialogChooseOverlay object.
         *
         * @param  parentFrame  DOCUMENT ME!
         */
        public JDialogChooseOverlay(Frame parentFrame) {
            super(parentFrame, true);
            init();
        }

        /**
         * the usual stuff.
         *
         * @param  e  ActionEvent
         */
        public void actionPerformed(ActionEvent e) {
            String ac = e.getActionCommand();

            if (ac.equals("OK")) {
                setVisible(false);

                int[] rows = table.getSelectedRows();

                if (isDicom) {
                    currentButton.setText((String) model.getValueAt(rows[0], 0));
                    currentName.setText((String) model.getValueAt(rows[0], 1));
                    // currentButton.setText(currentButton.getText() + (String) model.getValueAt(rows[0], 1));
                } else {
                    currentButton.setText((String) model.getValueAt(rows[0], 0));
                    currentName.setText((String) model.getValueAt(rows[0], 0));
                }

                table.clearSelection();
            } else if (ac.equals("Cancel")) {
                setVisible(false);
                table.clearSelection();
            }
        }

        /**
         * sets the button that will have its name updated when OK is clicked.
         *
         * @param  button  JButton
         */
        public void setButton(JButton button) {
            this.currentButton = button;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  field  DOCUMENT ME!
         */
        public void setName(JTextField field) {
            this.currentName = field;
        }

        /**
         * Do nothing on window close so we dont have to rebuild the dicom list.
         *
         * @param  event  WindowEvent
         */
        public void windowClosing(WindowEvent event) {
            setVisible(false);
        }

        /**
         * Build the table to be used in the dialog (dicom or image attributes).
         */
        private void buildTable() {

            model = new ViewTableModel();
            table = new JTable(model);
            table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
            table.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

            if (isDicom) {

                table.getTableHeader().addMouseListener(new HeaderListener());

                FileDicomKey key = null;
                FileDicomTagInfo info = null;
                Enumeration<FileDicomKey> e = DicomDictionary.getDicomTagTable().keys();

                model.addColumn("Tag");
                model.addColumn("Name");

                table.getColumn("Tag").setMinWidth(90);
                table.getColumn("Tag").setMaxWidth(90);
                table.getColumn("Name").setMinWidth(160);
                table.getColumn("Name").setMaxWidth(500);

                model.addRow(new String[] { BLANK_OVERLAY, "" });

                while (e.hasMoreElements()) {
                    key = e.nextElement();
                    info = DicomDictionary.getInfo(key);

                    if (info != null) {
                        model.addRow(new String[] {
                                         "(" + key.getGroup() + "," + key.getElement() + ")", info.getName()
                                     });
                    }
                }

                JDialogFileInfoDICOM.sort(model, 0, false, false);
            } else {
                model.addColumn("Image Attribute");

                for (int i = 0; i < attribStr.length; i++) {
                    model.addRow(new String[] { attribStr[i] });
                }
            }

        }

        /**
         * initialize the dialog.
         */
        private void init() {

            if (isDicom) {
                setTitle("Select DICOM tag for overlay");
            } else {
                setTitle("Choose attribute");
            }

            buildTable();
            scrollPane = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                         JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
            scrollPane.getVerticalScrollBar().setUnitIncrement(14);

            JPanel buttonPanel = new JPanel();
            buildOKButton();
            buttonPanel.add(OKButton);
            buildCancelButton();
            buttonPanel.add(cancelButton);

            this.getContentPane().add(scrollPane, BorderLayout.CENTER);
            this.getContentPane().add(buttonPanel, BorderLayout.SOUTH);

            pack();

            if (isDicom) {
                this.setSize(350, 500);
            } else {
                this.setSize(175, 450);
            }
        }

        /**
         * class that will sort the table when header is clicked.
         */
        private class HeaderListener implements MouseListener {

            /**
             * When the user clicks on a header, sorts the column.
             *
             * @param  e  event that triggered this method
             */

            public void mouseClicked(MouseEvent e) {
                Object source = e.getSource();
                Point p = e.getPoint();
                int col;

                if (source.equals(table.getTableHeader())) {
                    col = table.columnAtPoint(p);

                    if (col == 2) {
                        return;
                    }

                    if (e.isShiftDown()) {
                        JDialogFileInfoDICOM.sort(model, col, true, false);
                    } else {
                        JDialogFileInfoDICOM.sort(model, col, false, false);
                    }
                }
            }

            /**
             * Unchanged.
             *
             * @param  e  DOCUMENT ME!
             */
            @SuppressWarnings("unused")
            public void mouseDragged(MouseEvent e) { }

            /**
             * Unchanged.
             *
             * @param  e  DOCUMENT ME!
             */
            public void mouseEntered(MouseEvent e) { }

            /**
             * Unchanged.
             *
             * @param  e  DOCUMENT ME!
             */
            public void mouseExited(MouseEvent e) { }

            /**
             * Unchanged.
             *
             * @param  e  DOCUMENT ME!
             */
            public void mousePressed(MouseEvent e) { }

            /**
             * Unchanged.
             *
             * @param  e  DOCUMENT ME!
             */
            public void mouseReleased(MouseEvent e) { }
        }
    }

}
