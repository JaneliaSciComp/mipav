package gov.nih.mipav.view.dialogs;


import java.awt.*;
import java.awt.event.*;
import java.text.*;

import javax.swing.*;


/**
 * Confirmation Dialog giving user the choice to choose which ellipses to draw.
 *
 * @author   not attributable
 * @version  1.0
 */
public class JDialogHoughEllipseChoice extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private float xCenter[];
    
    private int xDimSource;
    
    private float yCenter[];
    
    private int yDimSource;
    
    private float r1[];
    
    private float r2[];
    
    private float theta[];
    
    private double minR1;
    
    private double maxR1;
    
    private double minR2;
    
    private double maxR2;
    
    private short count[];
    
    private boolean selectedEllipse[];
    
    private int numEllipsesFound;
    
    private JCheckBox selectedArray[];

    /** DOCUMENT ME! */
    private boolean okayPressed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogHoughEllipseChoice object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  xCenter
     * @param  xDimSource
     * @param  yCenter
     * @param  yDimSource
     * @param  r1
     * @param  maxR1
     * @param  count
     * @param  selectedEllipse
     */
    public JDialogHoughEllipseChoice(Frame theParentFrame, float xCenter[], int xDimSource, float yCenter[], int yDimSource,
                                  float r1[], double minR1, double maxR1, float r2[], double minR2, double maxR2, 
                                  float theta[], short count[], boolean selectedEllipse[]) {
        super(theParentFrame, true);
        this.xCenter = xCenter;
        this.xDimSource = xDimSource;
        this.yCenter = yCenter;
        this.yDimSource = yDimSource;
        this.r1 = r1;
        this.minR1 = minR1;
        this.maxR1 = maxR1;
        this.r2 = r2;
        this.minR2 = minR2;
        this.maxR2 = maxR2;
        this.theta = theta;
        this.count = count;
        this.selectedEllipse = selectedEllipse;
        numEllipsesFound = selectedEllipse.length;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Checks to see if the OK or Cancel buttons were pressed.
     *
     * @param  event  Event that triggered this function.
     */
    public void actionPerformed(ActionEvent event) {
        int i;

        if (event.getSource() == OKButton) {
            for (i = 0; i < numEllipsesFound; i++) {
                if (selectedArray[i].isSelected()) {
                    selectedEllipse[i] = true;
                } // if (selectedArray[i].isSelected())
                else {
                    selectedEllipse[i] = false;
                }
            } // for (i = 0; i < numEllipsesFound; i++)
            okayPressed = true;
        } // if (event.getSource() == OKButton)

        selectedArray = null;
        dispose();
    }

    /**
     * Check to see if line was selected or deselected
     *
     * @param  event  (combo box event)
     */
    public void itemStateChanged(ItemEvent event) {
        
    }

    /**
     * Was the okay button pressed.
     *
     * @return  boolean was okay pressed
     */
    public boolean okayPressed() {
        return okayPressed;
    }

    /**
     * Creates and displays dialog.
     */
    private void init() {
        DecimalFormat df = new DecimalFormat("0.#");
        int i;
        JLabel xCenterArrayLabel[] = new JLabel[numEllipsesFound];
        JLabel yCenterArrayLabel[] = new JLabel[numEllipsesFound];
        JLabel r1ArrayLabel[] = new JLabel[numEllipsesFound];
        JLabel r2ArrayLabel[] = new JLabel[numEllipsesFound];
        JLabel thetaArrayLabel[] = new JLabel[numEllipsesFound];
        JLabel countLabelArray[] = new JLabel[numEllipsesFound];
        selectedArray = new JCheckBox[numEllipsesFound];
        setTitle("Hough transform ellipse selection");
  
        JPanel createPanel = new JPanel(new GridBagLayout());
        createPanel.setBorder(buildTitledBorder("Select ellipse for display"));
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 20, 0, 0);
        
        JLabel xCenterLabel = new JLabel("x center(0-"+ String.valueOf(xDimSource-1) + ")");
        xCenterLabel.setFont(serif12);
        createPanel.add(xCenterLabel, gbc);
        
        gbc.gridx = 1;
        JLabel yCenterLabel = new JLabel("y center(0-" + String.valueOf(yDimSource-1) + ")");
        yCenterLabel.setFont(serif12);
        createPanel.add(yCenterLabel, gbc);
        
        gbc.gridx = 2;
        JLabel r1Label = new JLabel("r1(" + df.format(minR1)+ " - " + df.format(maxR1) + ")");
        r1Label.setFont(serif12);
        createPanel.add(r1Label, gbc);
        
        gbc.gridx = 3;
        JLabel r2Label = new JLabel("r2(" + df.format(minR2)+ " - " + df.format(maxR2) + ")");
        r2Label.setFont(serif12);
        createPanel.add(r2Label, gbc);
        
        gbc.gridx = 4;
        JLabel thetaLabel = new JLabel("theta in degrees ");
        thetaLabel.setFont(serif12);
        createPanel.add(thetaLabel, gbc);
        
        gbc.gridx = 5;
        JLabel countLabel = new JLabel("Points counted");
        countLabel.setFont(serif12);
        createPanel.add(countLabel, gbc);
        
        gbc.gridx = 6;
        JLabel selectLabel = new JLabel("Select ellipses");
        selectLabel.setFont(serif12);
        createPanel.add(selectLabel, gbc);
        
        for (i = 0; i < numEllipsesFound; i++) {
            gbc.gridx = 0;
            gbc.gridy++;
            xCenterArrayLabel[i] = new JLabel(df.format(xCenter[i]));
            xCenterArrayLabel[i].setFont(serif12);
            createPanel.add(xCenterArrayLabel[i], gbc);
            
            gbc.gridx = 1;
            yCenterArrayLabel[i] = new JLabel(df.format(yCenter[i]));
            yCenterArrayLabel[i].setFont(serif12);
            createPanel.add(yCenterArrayLabel[i], gbc);
            
            gbc.gridx = 2;
            r1ArrayLabel[i] = new JLabel(df.format(r1[i]));
            r1ArrayLabel[i].setFont(serif12);
            createPanel.add(r1ArrayLabel[i], gbc);
            
            gbc.gridx = 3;
            r2ArrayLabel[i] = new JLabel(df.format(r2[i]));
            r2ArrayLabel[i].setFont(serif12);
            createPanel.add(r2ArrayLabel[i], gbc);
            
            gbc.gridx = 4;
            thetaArrayLabel[i] = new JLabel(df.format(theta[i]* 180.0/Math.PI));
            thetaArrayLabel[i].setFont(serif12);
            createPanel.add(thetaArrayLabel[i], gbc);
            
            gbc.gridx = 5;
            countLabelArray[i] = new JLabel(String.valueOf(count[i]));
            countLabelArray[i].setFont(serif12);
            createPanel.add(countLabelArray[i], gbc);
            
            gbc.gridx = 6;
            selectedArray[i] = new JCheckBox("");
            selectedArray[i].setSelected(false);
            selectedArray[i].setFont(serif12);
            createPanel.add(selectedArray[i], gbc);
            
        } // for (i = 0; i < numEllipsesFound; i++)

        
        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);

        mainDialogPanel.add(createPanel);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }
}
