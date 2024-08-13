package org.janelia.mipav;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.Dialog.ModalityType;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

public class BatchProcessLogFrame implements ActionListener {
	// variables
	JFrame frame;
	JButton buttonOk, buttonCopyAll, buttonBrowse;
	JDialog noDupAnnDialog;
	JTextArea textArea;
	JFileChooser fileChooser;
	File file, directory;

	// Constructor
	public BatchProcessLogFrame() {
		this.frame = new JFrame("Batch Process Log");

		createGUI();
	}

	private void createGUI() {
		JOptionPane panelNoDupAnnDialog;
		
		frame.setLayout(new GridBagLayout());

		// create buttons and add top actionListener
		buttonOk = new JButton("Ok");
		buttonOk.addActionListener(this);
		buttonCopyAll = new JButton("Copy");
		buttonCopyAll.addActionListener(this);
		buttonBrowse = new JButton("Save");
		buttonBrowse.addActionListener(this);

		// a dialog pop out when there is no content in the textArea
		panelNoDupAnnDialog = new JOptionPane("No duplicated annotations are found!");
		noDupAnnDialog = panelNoDupAnnDialog.createDialog("Dialog");
		noDupAnnDialog.setModalityType(ModalityType.APPLICATION_MODAL);

		// Create TextArea
		textArea = new JTextArea(20, 70);
		textArea.setEditable(true);
		JScrollPane scrollPane = new JScrollPane(textArea);
		// Add Components to this panel.
		GridBagConstraints c = new GridBagConstraints();
		// Set GridLayout for textArea
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 3;
		c.fill = GridBagConstraints.BOTH; // fill both gridx and gridy
		c.weightx = 1; // distribute space
		c.weighty = 1;
		frame.add(scrollPane, c);

		// Set GridLayout for buttonCopyAll
		c.fill = GridBagConstraints.VERTICAL;
		c.anchor = GridBagConstraints.WEST;
		c.gridx = 0;
		c.gridy = 1;
		c.gridheight = 1;
		c.gridwidth = 1;
		c.weightx = 1; 
		c.weighty = 0;
		frame.add(buttonCopyAll, c); 

		// Anchor the buttonBrowse to the right (EAST) of the frame
		c.anchor = GridBagConstraints.EAST;
		c.fill = GridBagConstraints.NONE;
		c.gridx = 2;
		c.gridy = 1;
		c.gridheight = 1; 
		c.weightx = 1;
		frame.add(buttonBrowse, c);

		// Anchor the "OK" button to the CENTER of the frame
		c.anchor = GridBagConstraints.CENTER;
		c.fill = GridBagConstraints.NONE;
		c.gridx = 1;
		c.gridy = 2;
		c.gridheight = 2;
		c.weightx = 1;
		frame.add(buttonOk, c);

		// Display the frame.
		frame.setBounds(80, 80, 80, 80);
		frame.pack();
		frame.setVisible(true);

		fileChooser = new JFileChooser();
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		String newline = "\n";

		String data = textArea.getText().trim(); // read contents of text area into 'data'
		if (!data.equals("")) {
			if (e.getSource() == buttonOk) {
				frame.dispose();
			} else if (e.getSource() == buttonCopyAll) {
				// select all and copy to Clipboard
				StringSelection stringSelection = new StringSelection(textArea.getText());
				Clipboard clpbrd = Toolkit.getDefaultToolkit().getSystemClipboard();
				clpbrd.setContents(stringSelection, null);
				JOptionPane.showMessageDialog(null, "Texts are copied to clipboard!");
			} else if (e.getSource() == buttonBrowse) {
				fileChooser.setSelectedFile(new File("Duplicated_Annotations.txt"));

				int returnVal = fileChooser.showSaveDialog(frame);
				if (returnVal == JFileChooser.APPROVE_OPTION) {
					file = fileChooser.getSelectedFile();
					// save to file
					try (FileWriter fw = new FileWriter(fileChooser.getSelectedFile())) {
						fw.write(textArea.getText());
						textArea.append("\n\n  <<" + file.getName() + ">> is saved to: "
								+ fileChooser.getSelectedFile().getAbsolutePath() + newline);
					} catch (FileNotFoundException ex) {
						JOptionPane panelSaveFailedDialog = new JOptionPane("File save failed! " + "\n" + ex);
						JDialog saveFailedDialog = panelSaveFailedDialog.createDialog("Dialog");
						saveFailedDialog.setModalityType(ModalityType.APPLICATION_MODAL);
						saveFailedDialog.setVisible(true);
					
					} catch (Exception ex) {
						ex.printStackTrace();
					}
				}
			}
		} else {
			if (e.getSource() == buttonOk) {
				frame.dispose();
			} else {
				noDupAnnDialog.setVisible(true);
				System.out.println("no duplicated annotations.");
			}
		}
		// Make sure the new text is visible, even if there
		// was a selection in the text area.
		textArea.setCaretPosition(textArea.getDocument().getLength());
	}

	public void appendMessage(String str) {
		textArea.append(str);
	}

	public void setCurrentDirectory(String opDirectory) {
		directory = new File(opDirectory);
		fileChooser.setCurrentDirectory(directory);
	}
}
