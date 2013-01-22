package gov.nih.mipav.view.dialogs;


import java.awt.*;
import java.awt.event.*;
import java.text.*;

import javax.swing.*;


/**
 * Confirmation Dialog giving user the choice to choose which parabolas to draw.
 *
 * @author   not attributable
 * @version  1.0
 */
public class JDialogHoughParabolaChoice extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private float xvTable[];
    
    private int xDim;
    
    private float yvTable[];
    
    private int yDim;
    
    private double phiTable[];
    
    private int phiBins;
    
    private float pTable[];
    
    private float pMin;
    
    private float pMax;
    
    private int countTable[];
    
    private boolean selectedParabola[];
    
    private int numParabolasFound;
    
    private JCheckBox selectedArray[];

    /** DOCUMENT ME! */
    private boolean okayPressed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogHoughParabolaChoice object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  xvTable
     * @param  xDim
     * @param  yvTable
     * @param  yDim
     * @param  phiTable
     * @param  phiBins
     * @param  pTable
     * @param  pMin
     * @param  pMax
     * @param  countTable
     * @param  selectedParabola
     */
    public JDialogHoughParabolaChoice(Frame theParentFrame, float xvTable[], int xDim, float yvTable[], int yDim,
                                  double phiTable[], int phiBins, float pTable[], float pMin, float pMax,
                                  int countTable[], boolean selectedParabola[]) {
        super(theParentFrame, true);
        this.xvTable = xvTable;
        this.xDim = xDim;
        this.yvTable = yvTable;
        this.yDim = yDim;
        this.phiTable = phiTable;
        this.phiBins = phiBins;
        this.pTable = pTable;
        this.pMin = pMin;
        this.pMax = pMax;
        this.countTable = countTable;
        this.selectedParabola = selectedParabola;
        numParabolasFound = selectedParabola.length;
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
            for (i = 0; i < numParabolasFound; i++) {
                if (selectedArray[i].isSelected()) {
                    selectedParabola[i] = true;
                } // if (selectedArray[i].isSelected())
                else {
                    selectedParabola[i] = false;
                }
            } // for (i = 0; i < numParabolasFound; i++)
            okayPressed = true;
        } else { // if (event.getSource() == OKButton)
            super.actionPerformed(event);
        }
        
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
        JLabel xvLabelArray[] = new JLabel[numParabolasFound];
        JLabel yvLabelArray[] = new JLabel[numParabolasFound];
        JLabel phiLabelArray[] = new JLabel[numParabolasFound];
        JLabel pLabelArray[] = new JLabel[numParabolasFound];
        JLabel countLabelArray[] = new JLabel[numParabolasFound];
        selectedArray = new JCheckBox[numParabolasFound];
        setTitle("Hough transform parabola selection");
  
        JPanel createPanel = new JPanel(new GridBagLayout());
        createPanel.setBorder(buildTitledBorder("Select parabolas for display"));
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 20, 0, 0);
        
        JLabel xvLabel = new JLabel("x vertex(0-"+ String.valueOf(xDim-1) + ")");
        xvLabel.setFont(serif12);
        createPanel.add(xvLabel, gbc);
        
        gbc.gridx = 1;
        JLabel yvLabel = new JLabel("y vertex(0-" + String.valueOf(yDim-1) + ")");
        yvLabel.setFont(serif12);
        createPanel.add(yvLabel, gbc);
        
        gbc.gridx = 2;
        JLabel phiLabel;
        if (phiBins > 1) {
            phiLabel = new JLabel("phi(0 - 360)");
        }
        else {
            phiLabel = new JLabel("constant phi");
        }
        phiLabel.setFont(serif12);
        createPanel.add(phiLabel, gbc);
        
        gbc.gridx = 3;
        JLabel pLabel = new JLabel("vertex to focus("+ String.valueOf(pMin) + "-" + String.valueOf(pMax) + ")");
        pLabel.setFont(serif12);
        createPanel.add(pLabel, gbc);
        
        gbc.gridx = 4;
        JLabel countLabel = new JLabel("Points counted");
        countLabel.setFont(serif12);
        createPanel.add(countLabel, gbc);
        
        gbc.gridx = 5;
        JLabel selectLabel = new JLabel("Select parabolas");
        selectLabel.setFont(serif12);
        createPanel.add(selectLabel, gbc);
        
        for (i = 0; i < numParabolasFound; i++) {
            gbc.gridx = 0;
            gbc.gridy++;
            xvLabelArray[i] = new JLabel(df.format(xvTable[i]));
            xvLabelArray[i].setFont(serif12);
            createPanel.add(xvLabelArray[i], gbc);
            
            gbc.gridx = 1;
            yvLabelArray[i] = new JLabel(df.format(yvTable[i]));
            yvLabelArray[i].setFont(serif12);
            createPanel.add(yvLabelArray[i], gbc);
            
            gbc.gridx = 2;
            phiLabelArray[i] = new JLabel(df.format(phiTable[i]* 180.0/Math.PI));
            phiLabelArray[i].setFont(serif12);
            createPanel.add(phiLabelArray[i], gbc);
            
            gbc.gridx = 3;
            pLabelArray[i] = new JLabel(df.format(pTable[i]));
            pLabelArray[i].setFont(serif12);
            createPanel.add(pLabelArray[i], gbc);
            
            gbc.gridx = 4;
            countLabelArray[i] = new JLabel(String.valueOf(countTable[i]));
            countLabelArray[i].setFont(serif12);
            createPanel.add(countLabelArray[i], gbc);
            
            gbc.gridx = 5;
            selectedArray[i] = new JCheckBox("");
            selectedArray[i].setSelected(false);
            selectedArray[i].setFont(serif12);
            createPanel.add(selectedArray[i], gbc);
            
        } // for (i = 0; i < numParabolasFound; i++)

        
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
