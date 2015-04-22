import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;


public class PlugInDialogNeuronalActin extends JDialogStandalonePlugin implements AlgorithmInterface {

	/**
	 * 
	 */
	private static final long serialVersionUID = 7054211453483441130L;

	private JTextField swcField;

	private JTextField imageField;

	private JTextPane textArea;

	private JTextField actinField;

	private final SimpleAttributeSet blackText;

	private final SimpleAttributeSet redText;

	private final String[] acceptedImTypes = new String[] {".ics"};

	public PlugInDialogNeuronalActin() {
		super();

		blackText = new SimpleAttributeSet();
		StyleConstants.setFontFamily(blackText, "Serif");
		StyleConstants.setFontSize(blackText, 12);

		redText = new SimpleAttributeSet(blackText);
		StyleConstants.setForeground(redText, Color.red.darker());

		init();

		String version = "1.0.0";
		String updated = "4/21/15";
		append("Version " + version, blackText);
		append("Last updated " + updated, blackText);
		append("-----------------------------------------", blackText);
	}

	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if (command.equals("ok")) {
			callAlgorithm();
		} else if (command.startsWith("Browse")) {
			if (command.equals("Browse")) {
				chooseDir(true);
			} else {
				chooseDir(false);
			}
		} else if (command.equals("cancel")) {
			if (isExitRequired()) {
				ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
			} else {
				dispose();
			}
		}
	}

	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		append("-----------------------------------------", blackText);
	}

	protected void callAlgorithm() {
		String swcFile = swcField.getText();
		String imgFile = imageField.getText();

		if (!(new File(swcFile).exists())) {
			append("The provided SWC file does not exist.", redText);
			return;
		}

		if (!(new File(imgFile).exists())) {
			append("The provided image file does not exist.", redText);
			return;
		}

		PlugInAlgorithmNeuronalActin alg = new PlugInAlgorithmNeuronalActin(imgFile, swcFile, textArea);
		alg.addListener(this);
		if (actinField.isEnabled()) {
			int channel;
			try {
				channel = Integer.valueOf(actinField.getText());
			} catch (NumberFormatException e) {
				e.printStackTrace();
				append("The input channel is not an integer", redText);
				return;
			}
			if (channel >= 0 && channel < 4) {
				alg.setActinChannel(channel);
			} else {
				append("The input channel is not between 0 and 3, inclusive", redText);
				return;
			}
		}
		if (isRunInSeparateThread()) {
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				append("A thread is already running on this object", redText);
			}
		} else {
			alg.run();
		}

	}

	private void append(String message, AttributeSet a) {
		Document doc = textArea.getDocument();
		try {
			doc.insertString(doc.getLength(), message + "\n", a);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}

		textArea.setCaretPosition(doc.getLength());
	}

	private void chooseDir(boolean chooseIV) {
		String dirText = Preferences.getImageDirectory();

		FileFilter ivFilter = new FileFilter() {

			@Override
			public boolean accept(File pathname) {
				if (pathname.isDirectory())
					return true;
				String name = pathname.getName();
				int index = name.lastIndexOf(".");
				if (index < 0)
					return false;
				String fileExt = name.substring(index);
				if (fileExt.equalsIgnoreCase(".swc"))
					return true;
				else
					return false;
			}

			@Override
			public String getDescription() {
				return "SWC File (.swc)";
			}

		};

		FileFilter imFilter = new FileFilter() {
			@Override
			public boolean accept(File pathname) {
				if (pathname.isDirectory())
					return true;
				String name = pathname.getName();
				int index = name.lastIndexOf(".");
				if (index < 0)
					return false;
				String fileExt = name.substring(index);

				for (int i = 0; i < acceptedImTypes.length; i++) {
					if (fileExt.equalsIgnoreCase(acceptedImTypes[i])) {
						return true;
					}
				}

				return false;
			}

			@Override
			public String getDescription() {
				return "Imaris Image (.ics)";
			}
		};

		FileFilter filter = chooseIV ? ivFilter : imFilter;

		JFileChooser fileChooser = new JFileChooser(dirText);
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.addChoosableFileFilter(filter);
		fileChooser.setFileFilter(filter);
		int returnVal = fileChooser.showOpenDialog(this);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			String selection = fileChooser.getSelectedFile().getAbsolutePath();
			if (chooseIV) {
				swcField.setText(selection);
			} else {
				imageField.setText(selection);
			}
			Preferences.setImageDirectory(new File(selection));
		}
	}

	private void init() {

		setTitle("Neuronal Actin Stats");

		getContentPane().removeAll();
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));

		JPanel mainPanel = new JPanel(new GridBagLayout());
		mainPanel.setForeground(Color.black);

		JLabel fileLabel = new JLabel("Input SWC File");
		fileLabel.setFont(serif12B);

		swcField = new JTextField(30);
		swcField.setFont(serif12);

		JButton browseButton = new JButton("Browse");
		browseButton.setFont(serif12);
		browseButton.addActionListener(this);

		JLabel imLabel = new JLabel("Input Actin Image");
		imLabel.setFont(serif12B);

		imageField = new JTextField(30);
		imageField.setFont(serif12);

		JButton browseImage = new JButton("Browse");
		browseImage.setFont(serif12);
		browseImage.setActionCommand("BrowseImage");
		browseImage.addActionListener(this);

		JLabel actinLabel = new JLabel("Actin Channel (0-3)");
		actinLabel.setFont(serif12B);

		actinField = new JTextField(3);
		actinField.setFont(serif12);
		actinField.setText("0");
		actinField.setHorizontalAlignment(JTextField.RIGHT);

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.weighty = 0;
		gbc.insets = new Insets(5, 5, 5, 5);
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;

		mainPanel.add(fileLabel, gbc);

		gbc.gridy = 1;
		gbc.weightx = 1;
		gbc.gridwidth = 2;

		mainPanel.add(swcField, gbc);

		gbc.gridx = 2;
		gbc.weightx = 0;
		gbc.gridwidth = 1;

		mainPanel.add(browseButton, gbc);

		gbc.gridx = 0;
		gbc.gridy++;

		mainPanel.add(imLabel, gbc);

		gbc.gridy++;
		gbc.weightx = 1;
		gbc.gridwidth = 2;

		mainPanel.add(imageField, gbc);

		gbc.gridx = 2;
		gbc.weightx = 0;
		gbc.gridwidth = 1;

		mainPanel.add(browseImage, gbc);

		gbc.gridx = 0;
		gbc.gridy++;

		mainPanel.add(actinLabel, gbc);

		gbc.gridx = 1;
		gbc.fill = GridBagConstraints.NONE;
		mainPanel.add(actinField, gbc);

		getContentPane().add(mainPanel);

		buildOKCancelButtons();

		OKButton.setActionCommand("ok");
		cancelButton.setActionCommand("cancel");

		JPanel buttonPanel = new JPanel();
		buttonPanel.setForeground(Color.black);
		buttonPanel.add(OKButton, BorderLayout.WEST);
		buttonPanel.add(cancelButton, BorderLayout.EAST);

		getContentPane().add(buttonPanel);

		JPanel debugPanel = new JPanel();
		debugPanel.setLayout(new BoxLayout(debugPanel, BoxLayout.PAGE_AXIS));;
		debugPanel.setForeground(Color.black);
		debugPanel.setBorder(new TitledBorder(BorderFactory.createEmptyBorder(), "Debugging Output"));

		JPanel textPanel = new JPanel(new BorderLayout());
		textArea = new JTextPane();
		textPanel.add(textArea, BorderLayout.CENTER);
		JScrollPane scrollPane = new JScrollPane(textPanel);
		scrollPane.setPreferredSize(new Dimension(100, 200));
		scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		debugPanel.add(scrollPane);

		getContentPane().add(debugPanel);

		pack();
		setVisible(true);
		System.gc();
	}



}
