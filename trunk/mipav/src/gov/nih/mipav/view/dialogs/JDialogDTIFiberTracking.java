package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.WindowListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class JDialogDTIFiberTracking extends JDialogBase implements AlgorithmInterface, WindowListener {
	
	private JTextField tensorImageTextField, outputDirTextField;
	
	
	
	
	
	
	
	public JDialogDTIFiberTracking() {
		super(ViewUserInterface.getReference().getMainFrame(), false);
		init();
	}
	
	
	
	private void init() {
		setForeground(Color.black);
        setTitle("Fiber Tracking");
        
        GridBagConstraints gbc = new GridBagConstraints();
        
        JPanel mainPanel = new JPanel(new GridBagLayout());
        
        JLabel tensorImageLabel = new JLabel("Tensor Image");
        JLabel outputDirLabel = new JLabel("Output Dir");
        
        tensorImageTextField = new JTextField(20);
        tensorImageTextField.setEditable(false);
        tensorImageTextField.setBackground(Color.white);
        
        outputDirTextField = new JTextField(20);
        outputDirTextField.setEditable(false);
        outputDirTextField.setBackground(Color.white);
        
        JButton tensorBrowseButton = new JButton("Browse");
        tensorBrowseButton.addActionListener(this);
        tensorBrowseButton.setActionCommand("tensorBrowse");
        
        JButton outputDirBrowseButton = new JButton("Browse");
        outputDirBrowseButton.addActionListener(this);
        outputDirBrowseButton.setActionCommand("outputDirBrowse");
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(15,5,5,15);
        gbc.gridwidth = 1;
        
        mainPanel.add(tensorImageLabel, gbc);
        
        gbc.gridx = 1;
        mainPanel.add(tensorImageTextField, gbc);
        
        gbc.gridx = 2;
        mainPanel.add(tensorBrowseButton, gbc);
        
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        
        mainPanel.add(outputDirLabel, gbc);
        
        gbc.gridx = 1;
        mainPanel.add(outputDirTextField, gbc);
        
        gbc.gridx = 2;
        mainPanel.add(outputDirBrowseButton, gbc);
        
        
        JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        
        
        
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        pack();
        setMinimumSize(getSize());
        //setResizable(false);
        setVisible(true);
        
        
        
        
        
	}
	

	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub

	}

	public void actionPerformed(ActionEvent e) {
		// TODO Auto-generated method stub

	}

}
