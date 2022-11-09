package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;


/**
 * This class simply shuffle the H5 list randomly. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogShuffleList extends JDialogBase implements
		AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	/** saved 2D slices atlas dir. */
	private JTextField textFieldSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	private RandomNumberGen xRandom;
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogShuffleList(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		init();
		setVisible(true);

	}

	/**
	 * dispose memory
	 * */
	public void disposeLocal() {
	
	}

	/**
	 * Dialog local actionPerformed handler.
	 */
	public void actionPerformed(ActionEvent event) {

		String command = event.getActionCommand();
		if (command.equals("OK")) {
			shuffleList();
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("Help")) {
			
		} else if (command.equals("ChooseKeyImageDir")) {
			
		} else if (command.equals("ChooseSaveImageDir")) {
			
		}
	}

	/**
	 * Read 3D images atlas directory.
	 */
	private void shuffleList() {

		Vector<String> nameList = new Vector<String>();
		String fileName = "/scratch/miccai/3DUnet_approach/randomSample/FileList/train_3DUnet.lst";
		Vector<String> nameListWrite = new Vector<String>();
		
		try {
			BufferedReader in = new BufferedReader(new FileReader(fileName));
			String line;
			while ((line = in.readLine()) != null) {
				System.err.println(line);
				nameList.add(line);
			}
			in.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		System.err.println("finish read");
		
		int size = nameList.size();
		System.err.println("size = " + size);
		xRandom = new RandomNumberGen();
		int samples = 0;
		boolean [] map = new boolean[size]; 
		
		while ( true ) {	
			
			if ( samples > 30000 ) break;
			int rand_index = xRandom.genUniformRandomNum(0, size);
			if ( map[rand_index] == true ) continue;
			String name = nameList.get(rand_index);
			System.err.println("name = " + name);
		    nameListWrite.add(name);
		    map[rand_index] = true;
		    samples++;
		    
		}
	     
		try {
			
			PrintWriter fileWriter = new PrintWriter(new FileWriter("/scratch/miccai/3DUnet_approach/randomSample/FileList/train_3DUnet_shuffle.lst"));
			int len = nameListWrite.size();
			int i;
	        for (i = 0; i < len; i++) {
	        	fileWriter.write(nameListWrite.get(i) + "\n");
			}
			
			fileWriter.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	

	/**
	 * This method is required if the AlgorithmPerformed interface is
	 * implemented. It is called by the algorithms when it has completed or
	 * failed to to complete, so that the dialog can be display the result image
	 * and/or clean up.
	 * 
	 * @param algorithm
	 *            Algorithm that caused the event.
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {

	}



	/**
	 * Initial panel
	 */
	public void init() {

		JPanel mainPanel;

		mainPanel = new JPanel();
		mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
		mainPanel.setLayout(new BorderLayout());

		mainPanel.add(buildButtons(), BorderLayout.SOUTH);

		getContentPane().add(mainPanel);
		pack();
		setResizable(true);
	}


}