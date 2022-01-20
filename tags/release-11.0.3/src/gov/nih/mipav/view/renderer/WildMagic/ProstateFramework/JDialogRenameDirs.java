package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.io.*;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import java.util.*;

import javax.swing.*;


public class JDialogRenameDirs extends JDialogBase implements AlgorithmInterface {
	
	/** The main user interface. */
	private ViewUserInterface UI;
	
	private JPanel imageSelectionPanel;
	private JPanel buttonPanel;
	
	// axis region
	private JComboBox axisList;
	private JLabel labelAxis;
	private static int Axial = 0;
	
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;
	
	
	// key images variables
	private JFileChooser keyImageChooser = new JFileChooser();
	private String keyImageDirectory;

		
	public JDialogRenameDirs(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		init();
	}
	
	public void algorithmPerformed(AlgorithmBase algorithm) {      
	}

	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		if (command.equals("OK")) {
			// groupImages();
		} else if (command.equals("SetAxis")) {
			// axis = axisList.getSelectedIndex();
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("ChooseKeyImageDir")) {
			readKeyImageDir();
		} else if (command.equals("ChooseGroupDir")) {
			// selectGroupDir();
		}
	}

	public void init() {

		setTitle("Rename image dirs");

		final JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

		final GridBagConstraints gbc = new GridBagConstraints();

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.EAST;

		imageSelectionPanel = new JPanel();
		imageSelectionPanel.setLayout(new GridLayout(2, 3));
		imageSelectionPanel.setBorder(buildTitledBorder("Groups"));

		gbc.gridx = 0;
		gbc.gridy = 0;
		
		 // axis label
		String[] axisStrings = { "Axial", "Saggital", "Coronal" };

	    axisList = new JComboBox(axisStrings);
		axisList.setSelectedIndex(0);
		axisList.setActionCommand("SetAxis");
		axisList.addActionListener(this);
		
		labelAxis = new JLabel("Axis: ");
		labelAxis.setFont(serif12);
		labelAxis.setForeground(Color.black);

		imageSelectionPanel.add(labelAxis, gbc);
		
		gbc.gridx = 1;
		imageSelectionPanel.add(axisList, gbc);
		
		gbc.gridx = 2;
		JLabel emptyLabel = new JLabel("");
		imageSelectionPanel.add(emptyLabel, gbc);
		
		// Key image directory
		gbc.gridx = 0;
		gbc.gridy = 1;
		labelKeyImage = new JLabel("Key Image Directory: ");
		labelKeyImage.setFont(serif12);
		labelKeyImage.setForeground(Color.black);

		imageSelectionPanel.add(labelKeyImage, gbc);

		textFieldKeyImage = new JTextField(20);
		textFieldKeyImage.setFont(serif12);

		gbc.gridx = 1;
		imageSelectionPanel.add(textFieldKeyImage, gbc);

		buttonKeyImage = new JButton("Choose");
		buttonKeyImage.addActionListener(this);
		buttonKeyImage.setActionCommand("ChooseKeyImageDir");
		buttonKeyImage.setFont(serif12B);
		buttonKeyImage.setPreferredSize(MipavUtil.defaultButtonSize);

		gbc.gridx = 2;
		imageSelectionPanel.add(buttonKeyImage, gbc);
        		
	    // button Panel
		buttonPanel = new JPanel();
		buttonPanel.setLayout(new GridLayout(1, 3));
		gbc.gridx = 0;
		gbc.gridy = 0;
		buttonPanel.add(buildOKButton(), gbc);
		gbc.gridy = 1;
		buttonPanel.add(buildCancelButton(), gbc);
		gbc.gridy = 2;
		buttonPanel.add(buildHelpButton(), gbc);

		mainPanel.add(imageSelectionPanel);
		mainPanel.add(buttonPanel);
		getContentPane().add(mainPanel);
		pack();
		setVisible(true);
	}
	
	private void readKeyImageDir() {
		String keyImageName;
		keyImageChooser.setDialogTitle("Open Key Images Directory");
		keyImageChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				final int returnValue = keyImageChooser.showOpenDialog(UI
				.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			keyImageName = keyImageChooser.getSelectedFile().getName();

			keyImageDirectory = String.valueOf(keyImageChooser
					.getCurrentDirectory())
					+ File.separatorChar
					+ keyImageName
					+ File.separatorChar;
			// UI.setDefaultDirectory(directory);
			textFieldKeyImage.setText(keyImageDirectory);

			File fileDir = new File(keyImageDirectory);
			System.err.println("check = " + keyImageDirectory);
			
			String[] children = fileDir.list();
			int len = children.length;
			for ( int i = 0; i < len; i++ ) {
				// String dir = keyImageDirectory + children[i] + File.separator;
				File fDir = new File(keyImageDirectory, children[i]);
				processDir(fDir);
			}
			
			// System.err.println("check = " + keyImageDirectory);
			// processDir(fileDir);
			setVisible(false);
		} else {
			return;
		}
		
	}

	private void processDir(File dir) {
		
		String[] childrenUnsorted = dir.list();
		String[] children = new String[childrenUnsorted.length];
		
		int len = 0;
		Hashtable<Integer, String> htable = new Hashtable<Integer, String>();
		for ( int i = 0; i < childrenUnsorted.length; i++ ) {
			String groupName = childrenUnsorted[i];
			int groupNumber = Integer.valueOf(groupName.substring(5, groupName.length()));
			htable.put(groupNumber, groupName);
			len++;
		}
		
		for ( int i = 0; i < len; i++ ) {
			children[i] = htable.get(i);
		}
		
		
		len = children.length-1;
		for ( int i = len; i >= 0; i-- ) {
			File file = new File(dir, children[i]);
			// System.err.println(file.toString());
			
			String groupName = children[i];
			int base = 0;
			int groupNumber = Integer.valueOf(groupName.substring(5, groupName.length()));
			// System.err.println(" groupNumber = " + groupNumber );
	
			String dirName = dir.toString();
			int backSlashIndex = dirName.lastIndexOf(File.separator);
			int sliceNumber = Integer.valueOf(dirName.substring(backSlashIndex+6, dirName.length()));
			System.err.println("sliceNumber = " + sliceNumber);
		   
			switch ( sliceNumber ) {
			case 3: 
				base = 3;
				break;
			case 4:
				base = 7;
				break;
			case 5:
				base = 19;
				break;
			case 6: 
				base = 48;
				break;
			case 7: 
				base = 65;
				break;
			case 8:
				base = 78;
				break;
			case 9:
				base = 88;
				break;
			case 10:
				base = 94;
				break;
			case 11:
			    base = 99;
			    break;
			case 12: 
				base = 100;
				break;
			case 13:
				base = 100;
				break;
			case 14:
				base = 99;
				break;
			case 15: 
				base = 92;
				break;
			case 16:
				base = 92;
				break;
			case 17: 
				base = 80;
				break;
			case 18:
				base = 48;
				break;
			case 19: 
				base = 28;
				break;
			case 20:
				base = 20;
				break;
			default:
				break;
			}
			String newDirName = dir.toString() + File.separator + "Group" + (groupNumber + base);
			System.err.println("newDirName = " + newDirName);
			// System.err.println("dirName = " + dir.toString() + children[i]);
			// File newDir = new File(newDirName);
			file.renameTo(new File(newDirName));
		   
		} 
	    System.err.println("finish");
		
	}
	
    
	  
}