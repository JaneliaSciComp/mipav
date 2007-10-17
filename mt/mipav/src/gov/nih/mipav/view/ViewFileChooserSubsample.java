package gov.nih.mipav.view;


import java.awt.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class ViewFileChooserSubsample extends JFileChooser {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5946259539592286508L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected JCheckBox chkSave;

    /** DOCUMENT ME! */
    protected JTextField txtResampleHeight;

    /** DOCUMENT ME! */
    protected JTextField txtResampleWidth;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ViewFileChooserSubsample object.
     */
    public ViewFileChooserSubsample() {
        this(null);
    }

    /**
     * Creates a new ViewFileChooserSubsample object.
     *
     * @param  defaultPath  DOCUMENT ME!
     */
    public ViewFileChooserSubsample(String defaultPath) {
        super(defaultPath);

        setFileSelectionMode(JFileChooser.FILES_ONLY);

        setAccessory(buildGUI());
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void approveSelection() {

        if (dimensionsOK()) {
            super.approveSelection();
        } else {
            MipavUtil.displayError("Cannot subsample this image because the subsampling dimensions are invalid.");
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  NumberFormatException  DOCUMENT ME!
     */
    public Dimension getSubsampleDimension() throws NumberFormatException {
        return new Dimension(Integer.parseInt(txtResampleWidth.getText()),
                             Integer.parseInt(txtResampleHeight.getText()));
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean saveDimensions() {
        return chkSave.isSelected();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected boolean dimensionsOK() {

        try {
            int height = Integer.parseInt(txtResampleHeight.getText());
            int width = Integer.parseInt(txtResampleWidth.getText());

            if ((height < 1) || (width < 1)) {
                return false;
            }

            return true;
        } catch (NumberFormatException nfe) {
            return false;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildGUI() {
        setDialogTitle("Choose multifile to subsample");

        JPanel northPanel = new JPanel();
        JLabel lblResamplingOptions = new JLabel("Subsampling options");
        northPanel.add(lblResamplingOptions);

        GridBagLayout gbLayout = new GridBagLayout();
        GridBagConstraints gbConstraints = new GridBagConstraints();

        JPanel centerPanel = new JPanel(gbLayout);

        JLabel lblResampleHeight = new JLabel("New height: ");
        JLabel lblResampleWidth = new JLabel("New width: ");

        txtResampleHeight = new JTextField(5);
        txtResampleWidth = new JTextField(5);

        Dimension defaultDimensions = Preferences.getSubsampleDimensions();

        if (defaultDimensions != null) {
            txtResampleHeight.setText(String.valueOf(defaultDimensions.height));
            txtResampleWidth.setText(String.valueOf(defaultDimensions.width));
        }

        gbLayout.setConstraints(lblResampleHeight, gbConstraints);
        centerPanel.add(lblResampleHeight);

        gbConstraints.gridx = 1;
        gbLayout.setConstraints(txtResampleHeight, gbConstraints);
        centerPanel.add(txtResampleHeight);

        gbConstraints.gridx = 0;
        gbConstraints.gridy = 1;
        gbLayout.setConstraints(lblResampleWidth, gbConstraints);
        centerPanel.add(lblResampleWidth);

        gbConstraints.gridx = 1;
        gbLayout.setConstraints(txtResampleWidth, gbConstraints);
        centerPanel.add(txtResampleWidth);

        centerPanel.setBorder(BorderFactory.createEtchedBorder());

        JPanel southPanel = new JPanel();
        chkSave = new JCheckBox("Remember these values");

        southPanel.add(chkSave);

        JPanel mainPanel = new JPanel(new BorderLayout());

        mainPanel.add(northPanel, BorderLayout.NORTH);
        mainPanel.add(centerPanel, BorderLayout.CENTER);
        mainPanel.add(southPanel, BorderLayout.SOUTH);

        return mainPanel;
    }
}
