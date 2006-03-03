package gov.nih.mipav.view;

import javax.swing.*;
import java.io.*;
import java.awt.*;

public class VolumePositionFrame extends JFrame implements CoordinateChangeListener
{
    protected ViewJFrameTriImage parentFrame;

    public JLabel labelXPos = new JLabel("");
    public JLabel labelYPos = new JLabel("");
    public JLabel labelZPos = new JLabel("");

    public JLabel labelXTal = new JLabel("");
    public JLabel labelYTal = new JLabel("");
    public JLabel labelZTal = new JLabel("");

    public JLabel scannerLabelX = new JLabel("");
    public JLabel scannerLabelY = new JLabel("");
    public JLabel scannerLabelZ = new JLabel("");

    public JLabel labelXRef = new JLabel("");
    public JLabel labelYRef = new JLabel("");
    public JLabel labelZRef = new JLabel("");

    public JCheckBox chkShowTalairachGrid = new JCheckBox("Show talairach grid");
    public JCheckBox chkShowTalairachGridMarkers = new JCheckBox("Show talairach grid markers");

    public VolumePositionFrame(ViewJFrameTriImage parentFrame)
    {
        super("Coordinates window");
        this.parentFrame = parentFrame;
        setSize(250, 225);
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        parentFrame.addCoordinateChangeListener(this);

        try
        {
            setIconImage(MipavUtil.getIconImage("3plane_16x16.gif"));
        }
        catch (FileNotFoundException fnfe)
        {}

        MipavUtil.centerOnScreen(this);

        getContentPane().setLayout(new GridLayout(1, 1));
        getContentPane().add(buildVolumePositionPanel());
    }

    public void coordinateChanged( int x, int y, int z )
    {
        labelXPos.setText(Integer.toString(x+1));
        labelYPos.setText(Integer.toString(y+1));
        labelZPos.setText(Integer.toString(z+1));
    }

    public void setShowTalairachGrid(boolean flag)
    {
        chkShowTalairachGrid.setSelected(flag);
        chkShowTalairachGridMarkers.setEnabled(flag);
    }

    public void setShowTalairachGridMarkers(boolean flag)
    {
        chkShowTalairachGridMarkers.setSelected(flag);
    }

    protected JPanel buildVolumePositionPanel()
    {
        JPanel volumePositionPanel = new JPanel();

        JTabbedPane tabbedPane = new JTabbedPane();

        GridBagLayout gbLayout = new GridBagLayout();
        GridBagConstraints gbConstraints = new GridBagConstraints();
        JPanel volumePanel = new JPanel(gbLayout);
        volumePanel.setBorder(BorderFactory.createTitledBorder("Volume coordinates"));

        gbConstraints.anchor = GridBagConstraints.EAST;
        volumePanel.add(new JLabel("X: "), gbConstraints);
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.gridx = 1;
        volumePanel.add(labelXPos, gbConstraints);

        gbConstraints.gridy = 1;
        gbConstraints.gridx = 0;
        gbConstraints.anchor = GridBagConstraints.EAST;
        volumePanel.add(new JLabel("Y: "), gbConstraints);
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.gridx = 1;
        volumePanel.add(labelYPos, gbConstraints);

        gbConstraints.gridy = 2;
        gbConstraints.gridx = 0;

        gbConstraints.anchor = GridBagConstraints.EAST;
        volumePanel.add(new JLabel("Z: "), gbConstraints);
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.gridx = 1;
        volumePanel.add(labelZPos, gbConstraints);

        gbLayout = new GridBagLayout();
        gbConstraints = new GridBagConstraints();
        JPanel talairachPanel = new JPanel(gbLayout);

        chkShowTalairachGrid = new JCheckBox("Show talairach grid");
        chkShowTalairachGrid.setActionCommand("ShowTalairachGrid");
        chkShowTalairachGrid.addActionListener(parentFrame);
        chkShowTalairachGrid.setFont(MipavUtil.font12B);
        chkShowTalairachGridMarkers = new JCheckBox("Show talairach grid markers");
        chkShowTalairachGridMarkers.setActionCommand("ShowTalairachGridmarkers");
        chkShowTalairachGridMarkers.setEnabled(chkShowTalairachGrid.isSelected());
        chkShowTalairachGridMarkers.addActionListener(parentFrame);
        chkShowTalairachGridMarkers.setFont(MipavUtil.font12B);

        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.weightx = 1;
        talairachPanel.add(chkShowTalairachGrid, gbConstraints);

        gbConstraints.gridy = 1;
        talairachPanel.add(chkShowTalairachGridMarkers, gbConstraints);

        GridBagLayout gbSubLayout = new GridBagLayout();
        GridBagConstraints gbSubConstraints = new GridBagConstraints();

        JPanel talairachSubPanel = new JPanel(gbSubLayout);
        talairachSubPanel.setBorder(BorderFactory.createTitledBorder("Talairach grid coordinates"));

        gbConstraints.gridy++;
        gbConstraints.gridwidth = 3;
        gbConstraints.fill = GridBagConstraints.BOTH;
        gbConstraints.weighty = 1;
        talairachPanel.add(talairachSubPanel, gbConstraints);

        gbSubConstraints.gridwidth = GridBagConstraints.RELATIVE;
        gbSubConstraints.gridy = 1;
        gbSubConstraints.weightx = 1;
        gbSubConstraints.anchor = GridBagConstraints.WEST;
        gbSubConstraints.insets = new Insets(0, 20, 0, 0);
        talairachSubPanel.add(new JLabel("X: "), gbSubConstraints);
        gbSubConstraints.gridx = 1;
        talairachSubPanel.add(labelXTal, gbSubConstraints);

        gbSubConstraints.gridx = GridBagConstraints.RELATIVE;
        gbSubConstraints.gridy = 2;
        talairachSubPanel.add(new JLabel("Y: "), gbSubConstraints);
        gbSubConstraints.gridx = 1;
        talairachSubPanel.add(labelYTal, gbSubConstraints);

        gbSubConstraints.gridx = GridBagConstraints.RELATIVE;
        gbSubConstraints.gridy = 3;
        talairachSubPanel.add(new JLabel("Z: "), gbSubConstraints);
        gbSubConstraints.gridx = 1;
        talairachSubPanel.add(labelZTal, gbSubConstraints);

        float[] tCoord = new float[3];
        parentFrame.getImageA().getScannerCoordLPS(parentFrame.getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getExtents()[0] / 2,
                               parentFrame.getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getExtents()[1] / 2,
                               parentFrame.getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getExtents()[2] / 2, tCoord);

        if (tCoord[0] < 0)
        {
            scannerLabelX.setText("R: ");
        }
        else
        {
            scannerLabelX.setText("L: ");
        }

        if (tCoord[1] < 0)
        {
            scannerLabelY.setText("A: ");
        }
        else
        {
            scannerLabelY.setText("P: ");
        }

        if (tCoord[2] < 0)
        {
            scannerLabelZ.setText("I: ");
        }
        else
        {
            scannerLabelZ.setText("S: ");
        }

        gbLayout = new GridBagLayout();
        gbConstraints = new GridBagConstraints();
        JPanel scannerPanel = new JPanel(gbLayout);
        scannerPanel.setBorder(BorderFactory.createTitledBorder("Scanner position"));

        gbConstraints.anchor = GridBagConstraints.EAST;
        scannerPanel.add(scannerLabelX, gbConstraints);
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.gridx = 1;
        scannerPanel.add(labelXRef, gbConstraints);

        gbConstraints.gridx = 0;
        gbConstraints.gridy = 1;
        gbConstraints.anchor = GridBagConstraints.EAST;
        scannerPanel.add(scannerLabelY, gbConstraints);
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.gridx = 1;
        scannerPanel.add(labelYRef, gbConstraints);

        gbConstraints.gridx = 0;
        gbConstraints.gridy = 2;
        gbConstraints.anchor = GridBagConstraints.EAST;
        scannerPanel.add(scannerLabelZ, gbConstraints);
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.gridx = 1;
        scannerPanel.add(labelZRef, gbConstraints);

        tabbedPane.add("Volume", volumePanel);
        tabbedPane.add("Scanner", scannerPanel);
        tabbedPane.add("Talairach", talairachPanel);

        volumePositionPanel.setLayout(new GridLayout(1, 1));
        volumePositionPanel.add(tabbedPane);

        return volumePositionPanel;
    }
}
