package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Confirmation Dialog giving user the choice to choose which lines to fill gaps on and
 * the maximum distance of the gap to be filled for each line..
 *
 * @author   not attributable
 * @version  1.0
 */
public class JDialogHoughLineChoice extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private int rhoArray[];
    
    private int n1;
    
    private int thetaArray[];
    
    private int n2;
    
    private int countArray[];
    
    private boolean selectedLine[];
    
    private float maxDistance[];
    
    private int numLinesFound;
    
    private JCheckBox selectedArray[];
    
    private JTextField distanceArray[];

    /** DOCUMENT ME! */
    private boolean okayPressed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogHoughLineChoice object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  rhoArray
     * @param  n1
     * @param  thetaArray
     * @param  n2
     * @param  countArray
     * @param  selectedLine
     * @param  maxDistance
     */
    public JDialogHoughLineChoice(Frame theParentFrame, int rhoArray[], int n1, int thetaArray[], int n2, int countArray[],
                                  boolean selectedLine[], float maxDistance[]) {
        super(theParentFrame, true);
        this.rhoArray = rhoArray;
        this.n1 = n1;
        this.thetaArray = thetaArray;
        this.n2 = n2;
        this.countArray = countArray;
        this.selectedLine = selectedLine;
        this.maxDistance = maxDistance;
        numLinesFound = selectedLine.length;
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
        String tmpStr;

        if (event.getSource() == OKButton) {
            for (i = 0; i < numLinesFound; i++) {
                if (selectedArray[i].isSelected()) {
                    selectedLine[i] = true;
                    tmpStr = distanceArray[i].getText();
                    if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
                        maxDistance[i] = Float.valueOf(tmpStr).floatValue();
                    } else {
                        MipavUtil.displayError("distance[" + String.valueOf(i) + "] must be >= 0.0");
                        distanceArray[i].requestFocus();
                        distanceArray[i].selectAll();

                        return;
                    }    
                } // if (selectedArray[i].isSelected())
                else {
                    selectedLine[i] = false;
                }
            } // for (i = 0; i < numLinesFound; i++)
            okayPressed = true;
        } else { // if (event.getSource() == OKButton)
            super.actionPerformed(event);
        }
        
        selectedArray = null;
        distanceArray = null;
        dispose();
    }

    /**
     * Check to see if line was selected or deselected
     *
     * @param  event  (combo box event)
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();
        int i;

        for (i = 0; i < numLinesFound; i++) {
            if (source == selectedArray[i]) {
                if (selectedArray[i].isSelected()) {
                    distanceArray[i].setEnabled(true);
                }
                else {
                    distanceArray[i].setEnabled(false);
                }
            }
        }
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
        int i;
        JLabel rhoLabelArray[] = new JLabel[numLinesFound];
        JLabel thetaLabelArray[] = new JLabel[numLinesFound];
        JLabel countLabelArray[] = new JLabel[numLinesFound];
        selectedArray = new JCheckBox[numLinesFound];
        distanceArray = new JTextField[numLinesFound];
        setTitle("Hough transform line selection");
  
        JPanel createPanel = new JPanel(new GridBagLayout());
        createPanel.setBorder(buildTitledBorder("Select lines and specify maximum gap distance"));
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 20, 0, 0);
        
        JLabel rhoLabel = new JLabel("rho(0-"+ String.valueOf(n1-1) + ")");
        rhoLabel.setFont(serif12);
        createPanel.add(rhoLabel, gbc);
        
        gbc.gridx = 1;
        JLabel thetaLabel = new JLabel("theta(0-" + String.valueOf(n2-1) + ")");
        thetaLabel.setFont(serif12);
        createPanel.add(thetaLabel, gbc);
        
        gbc.gridx = 2;
        JLabel countLabel = new JLabel("Points counted");
        countLabel.setFont(serif12);
        createPanel.add(countLabel, gbc);
        
        gbc.gridx = 3;
        JLabel selectLabel = new JLabel("Select lines");
        selectLabel.setFont(serif12);
        createPanel.add(selectLabel, gbc);
        
        gbc.gridx = 4;
        JLabel distanceLabel = new JLabel("Maximum gap distance");
        distanceLabel.setFont(serif12);
        createPanel.add(distanceLabel, gbc);
        
        for (i = 0; i < numLinesFound; i++) {
            gbc.gridx = 0;
            gbc.gridy++;
            rhoLabelArray[i] = new JLabel(String.valueOf(rhoArray[i]));
            rhoLabelArray[i].setFont(serif12);
            createPanel.add(rhoLabelArray[i], gbc);
            
            gbc.gridx = 1;
            thetaLabelArray[i] = new JLabel(String.valueOf(thetaArray[i]));
            thetaLabelArray[i].setFont(serif12);
            createPanel.add(thetaLabelArray[i], gbc);
            
            gbc.gridx = 2;
            countLabelArray[i] = new JLabel(String.valueOf(countArray[i]));
            countLabelArray[i].setFont(serif12);
            createPanel.add(countLabelArray[i], gbc);
            
            gbc.gridx = 3;
            selectedArray[i] = new JCheckBox("");
            selectedArray[i].setSelected(false);
            selectedArray[i].addItemListener(this);
            selectedArray[i].setFont(serif12);
            createPanel.add(selectedArray[i], gbc);
            
            gbc.gridx = 4;
            distanceArray[i] = new JTextField(8);
            distanceArray[i].setText("100.0");
            distanceArray[i].setFont(serif12);
            distanceArray[i].setEnabled(false);
            createPanel.add(distanceArray[i], gbc);
        } // for (i = 0; i < numLinesFound; i++)

        
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
