package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.file.DicomDictionary;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomSQ;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.view.MipavUtil;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.Vector;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;



/**
 * This class allows for DICOM tags to be selected.  Selected DICOM tags can either come from the overall DICOM dictionary or from the tags that exist in
 * a given image.
 * 
 * @author senseneyj
 *
 */
public class JDialogDicomTagSelector extends JDialogBase implements ListSelectionListener {

	/**
	 * Dialogs which implement this interface are eligible to use the JDialogDicomTagSelector for selecting DICOM tags within a given image file.
	 * This interface gives the parent's tag text field which is changed by the JDialogDicomTagSelector.
	 * 
	 * @author senseneyj
	 *
	 */
	public interface DicomTagSelectorImpl {
		public JTextField getTagListTextField(); 
	}
	
	/**For adding a top level dicom tag*/
	private static final String ADD_TAG = "Add";
	
	/**For adding a dicom tag contained within a sequence*/
	private static final String ADD_TAG_SEQ = "Add sequence tag";

	/**Closes the dialog*/
	private static final String CLOSE = "Close";

	/**For removing all tags from the main dialog*/
	private static final String CLEAR_TAGS = "Clear tags";
	
	/**Indicates tag value may exist but cannot be understood by the dialog. */
	private static final String UNKNOWN = "Unknown";
	
	/**Indicates tag value is a sequence tag*/
	private static final String SEQUENCE = "Sequence";

	/**Indicates tag is a private tag*/
	private static final String PRIVATE = "Private tag";
	
	/**Min/max size of all text boxes contained in scroll panes.*/
	private final Dimension SCROLL_PANE_SIZE = new Dimension(200, 20);

	/**List of all file's elements for each group of a FileDicomKey set*/
	private TreeMap<String, ArrayList<String>> groupToElement, groupToElementSeq;
	
	/**FileDicomTag to its name in the Dicom dictionary*/
	private TreeMap<String, String> keyToName, keyToNameSeq;

	/**FileDicomTag to its value in the file*/
	private TreeMap<String, String> keyToValue, keyToValueSeq;

	/**Panels used by this dialog box*/
	private JPanel tagSelectorPanel, tagInformationPanel, sequenceInformationPanel;

	/**Lists used to display available DICOM tags*/
	private JList groupList, elementList;
	
	/**Combo boxes used for sequence tags*/
	private JComboBox groupCombo, elementCombo;

	/**Buttons used by this dialog*/
	private JButton addButton, addButtonSeq, clearButton, closeButton;

	/**Name and property labels that describe a DICOM tag for a particular file*/
	private JLabel nameValue, nameValueSeq, propertyValue, propertyValueSeq;
	
	/**Original list of available DICOM tags*/
	private Hashtable<FileDicomKey, FileDicomTag> tagList;
	
	/** The parent dialog which receives text from this dicom tag editor */
	private DicomTagSelectorImpl parentPlugin;

	/** When this object exists as an embedded panel, this variable contains all necessary data elements. */
	private JPanel embeddedPanel;
	
	/**When a tag value exists, this label displays the text "Tag value:" */
	private JLabel propertyLabel;

	/**When a tag value exists, this panel displays the value */
	private JScrollPane propPane;

	/** Text fields for displaying/entering a group/element tag */
	private JTextField elementSeqText, groupSeqText, elementText, groupText;

	private TagInputListener k1, k2;
	
	public JDialogDicomTagSelector(Hashtable<FileDicomKey,FileDicomTag> tagList, JDialogBase parent, boolean isStandalone) {
		super(parent, false);
		
		this.tagList = tagList;
		
		if(parent instanceof DicomTagSelectorImpl) {
			this.parentPlugin = (DicomTagSelectorImpl)parent;
		} else {
			System.err.println("No valid tag editor parent was found, events from this dicom tag editor will not be received.");
		}
		
		//these data structures relate to the top level dicom tags
		this.groupToElement = new TreeMap<String, ArrayList<String>>();
		this.keyToName = new TreeMap<String, String>();
		this.keyToValue = new TreeMap<String, String>();
		this.setResizable(false);
		
		buildGroupElementMap(tagList);
		
		init(isStandalone);
	}


	/**
	 * Builds the panel that the tag dialog frame contains, and optionally builds 
	 * the standalone frame that encompasses all functionality of this class.
	 * @param isStandalone 
	 */
	public void init(boolean isStandalone) {
		if(isStandalone) {
			setForeground(Color.black);
	        setTitle("Tag Selector Tool");
		}
        
        tagSelectorPanel = buildTagSelectorPanel();
        
        tagInformationPanel = buildTagInfoPanel();
        
        sequenceInformationPanel = buildSequenceInfoPanel();
        
        if(isStandalone) {
	        add(tagSelectorPanel, BorderLayout.NORTH);
	        add(tagInformationPanel, BorderLayout.CENTER);
	        
	        pack();
        } else {
        	embeddedPanel = new JPanel();
        	embeddedPanel.add(tagSelectorPanel);
        	embeddedPanel.add(tagInformationPanel);
        }
	}
	
	private JPanel buildTagSelectorPanel() {
		GridBagLayout tagSelectorGridBagLayout = new GridBagLayout();
        GridBagConstraints selectorPanelConstraints = new GridBagConstraints();
        selectorPanelConstraints.anchor = GridBagConstraints.NORTH;

        JPanel tagSelectorPanel = new JPanel(tagSelectorGridBagLayout);
        tagSelectorPanel.setBorder(MipavUtil.buildTitledBorder("Select DICOM tags"));
        
        // Dialog column
        selectorPanelConstraints.gridx = 0;
        selectorPanelConstraints.gridy = 0;
        selectorPanelConstraints.gridheight = 4;
        selectorPanelConstraints.insets = new Insets(5, 5, 5, 5);
        selectorPanelConstraints.anchor = GridBagConstraints.CENTER;
        JLabel firstLabel = new JLabel("<html>Select<br>tags:</html>");
        tagSelectorPanel.add(firstLabel, selectorPanelConstraints);
        
        // Group Column
        selectorPanelConstraints.gridx = 1;
        selectorPanelConstraints.gridy = 0;
        selectorPanelConstraints.gridheight = 1;
        selectorPanelConstraints.anchor = GridBagConstraints.NORTHWEST;
        selectorPanelConstraints.insets = new Insets(5, 15, 5, 15);
        JLabel groupLabel = new JLabel("Group:");
        tagSelectorPanel.add(groupLabel, selectorPanelConstraints);
        
        selectorPanelConstraints.gridx = 2;
        selectorPanelConstraints.gridy = 0;
        selectorPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
        groupText = new JTextField();
        tagSelectorPanel.add(groupText, selectorPanelConstraints);
        
        selectorPanelConstraints.gridx = 1;
        selectorPanelConstraints.gridy = 1;
        selectorPanelConstraints.gridheight = 3;
        selectorPanelConstraints.gridwidth = 2;
        selectorPanelConstraints.anchor = GridBagConstraints.CENTER;
        Vector<String> vGroup;
        Collections.sort(vGroup = new Vector<String>(groupToElement.keySet()), new NumberComparator());
        groupList = new JList(vGroup);
        groupList.setSelectedIndex(0);
        groupList.setVisibleRowCount(4);
        groupList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        groupList.setMinimumSize(new Dimension(150, 94));
        groupList.setMaximumSize(new Dimension(150, 500));
        groupList.getSelectionModel().addListSelectionListener(this);
        JScrollPane scrollPaneGroup = new JScrollPane(groupList);
        scrollPaneGroup.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPaneGroup.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);    
        scrollPaneGroup.setMinimumSize(new Dimension(150, 94));
        scrollPaneGroup.setPreferredSize(new Dimension(150, 94));
        tagSelectorPanel.add(scrollPaneGroup, selectorPanelConstraints);
        k1 = new TagInputListener(groupText, groupList, this);
        groupText.setText(groupList.getModel().getElementAt(0).toString());
        
        // Element Column
        selectorPanelConstraints.gridx = 3;
        selectorPanelConstraints.gridy = 0;
        selectorPanelConstraints.gridwidth = 1;
        selectorPanelConstraints.gridheight = 1;
        selectorPanelConstraints.anchor = GridBagConstraints.NORTHWEST;
        JLabel elementLabel = new JLabel("Element:");
        tagSelectorPanel.add(elementLabel, selectorPanelConstraints);
        
        selectorPanelConstraints.gridx = 4;
        selectorPanelConstraints.gridy = 0;
        selectorPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
        elementText = new JTextField();
        tagSelectorPanel.add(elementText, selectorPanelConstraints);
        
        selectorPanelConstraints.gridx = 3;
        selectorPanelConstraints.gridy = 1;
        selectorPanelConstraints.gridwidth = 2;
        selectorPanelConstraints.gridheight = 3;
        selectorPanelConstraints.anchor = GridBagConstraints.CENTER;
        Vector<String> vElement;
        Collections.sort(vElement = new Vector<String>(groupToElement.get(vGroup.get(0))), new NumberComparator());
        elementList = new JList(vElement);
        elementList.setSelectedIndex(0);
        elementList.setVisibleRowCount(4);
        elementList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        elementList.setMinimumSize(new Dimension(150, 94));
        elementList.setMaximumSize(new Dimension(150, 500));
        elementList.getSelectionModel().addListSelectionListener(this);
        JScrollPane scrollPaneElement = new JScrollPane(elementList);
        scrollPaneElement.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPaneElement.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);    
        scrollPaneElement.setMinimumSize(new Dimension(150, 94));
        scrollPaneElement.setPreferredSize(new Dimension(150, 94));
        tagSelectorPanel.add(scrollPaneElement, selectorPanelConstraints);
        k2 = new TagInputListener(elementText, elementList, this);
        elementText.setText(elementList.getModel().getElementAt(0).toString());
        
        //Buttons Column
        selectorPanelConstraints.gridx = 5;
        selectorPanelConstraints.gridy = 1;
        selectorPanelConstraints.gridwidth = 1;
        selectorPanelConstraints.gridheight = 1;
        selectorPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
        selectorPanelConstraints.insets = new Insets(5, 5, 5, 5);
        addButton = new JButton("Add");
        addButton.setActionCommand(ADD_TAG);
        addButton.addActionListener(this);
        tagSelectorPanel.add(addButton, selectorPanelConstraints);
        
        selectorPanelConstraints.gridx = 5;
        selectorPanelConstraints.gridy = 2;
        clearButton = new JButton("Clear");
        clearButton.setActionCommand(CLEAR_TAGS);
        clearButton.addActionListener(this);
        tagSelectorPanel.add(clearButton, selectorPanelConstraints);
        
        selectorPanelConstraints.gridx = 5;
        selectorPanelConstraints.gridy = 3;
        closeButton = new JButton("Close");
        closeButton.setActionCommand(CLOSE);
        closeButton.addActionListener(this);
        tagSelectorPanel.add(closeButton, selectorPanelConstraints);
        
        return tagSelectorPanel;
	}
	
	private JPanel buildTagInfoPanel() {
		GridBagLayout tagInfoGridBagLayout = new GridBagLayout();
        GridBagConstraints infoPanelConstraints = new GridBagConstraints();
        infoPanelConstraints.anchor = GridBagConstraints.NORTH;

        JPanel tagInformationPanel = new JPanel(tagInfoGridBagLayout);
        tagInformationPanel.setBorder(MipavUtil.buildTitledBorder("DICOM tag information"));
        
        //tag name row
        infoPanelConstraints.gridx = 0;
        infoPanelConstraints.gridy = 0;
        infoPanelConstraints.insets = new Insets(5, 10, 5, 10);
        infoPanelConstraints.anchor = GridBagConstraints.NORTHWEST;
        infoPanelConstraints.fill = GridBagConstraints.BOTH;
        infoPanelConstraints.weightx = 0;
        JLabel nameLabel = new JLabel("Tag name:");
        tagInformationPanel.add(nameLabel, infoPanelConstraints);
        
        infoPanelConstraints.gridx = 1;
        infoPanelConstraints.gridy = 0;
        infoPanelConstraints.weightx = 1;
        String tagName = groupList.getSelectedValue().toString()+","+elementList.getSelectedValue().toString();
        nameValue = new JLabel(keyToName.get(tagName));
        JScrollPane namePane = new JScrollPane(nameValue);
        namePane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        namePane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        namePane.setBorder(null);
        namePane.setMinimumSize(SCROLL_PANE_SIZE);
        namePane.setPreferredSize(SCROLL_PANE_SIZE);
        namePane.setPreferredSize(SCROLL_PANE_SIZE);
        tagInformationPanel.add(namePane, infoPanelConstraints);
        
        //tag value row
        infoPanelConstraints.gridx = 0;
        infoPanelConstraints.gridy = 1;
        infoPanelConstraints.weightx = 0;
        propertyLabel = new JLabel("Tag value:");
        tagInformationPanel.add(propertyLabel, infoPanelConstraints);
        
        infoPanelConstraints.gridx = 1;
        infoPanelConstraints.gridy = 1;
        infoPanelConstraints.weightx = 1;
        propertyValue = new JLabel(keyToValue.get(tagName));
        propPane = new JScrollPane(propertyValue);
        propPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        propPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        propPane.setMinimumSize(SCROLL_PANE_SIZE);
        propPane.setMaximumSize(SCROLL_PANE_SIZE);
        propPane.setPreferredSize(SCROLL_PANE_SIZE);
        propPane.setBorder(null);
        tagInformationPanel.add(propPane, infoPanelConstraints);
        
        if(keyToValue.get(tagName).equals(UNKNOWN)) {
        	propPane.setVisible(false);
        	propertyLabel.setVisible(false);
        }
        
        return tagInformationPanel;
	}
	
	private JPanel buildSequenceInfoPanel() {
		GridBagLayout seqInfoGridBagLayout = new GridBagLayout();
		GridBagConstraints seqPanelConstraints = new GridBagConstraints();
		seqPanelConstraints.anchor = GridBagConstraints.NORTH;
		
		JPanel sequenceInformationPanel = new JPanel(seqInfoGridBagLayout);
		sequenceInformationPanel.setBorder(MipavUtil.buildTitledBorder("Sequence tag information"));
		
		// Dialog column
        seqPanelConstraints.gridx = 0;
        seqPanelConstraints.gridy = 0;
        seqPanelConstraints.weightx = 0;
        seqPanelConstraints.gridheight = 2;
        seqPanelConstraints.insets = new Insets(5, 5, 5, 5);
        seqPanelConstraints.anchor = GridBagConstraints.CENTER;
        JLabel firstLabel = new JLabel("Select tags:");
        sequenceInformationPanel.add(firstLabel, seqPanelConstraints);
        
        // Group Column
        seqPanelConstraints.gridx = 1;
        seqPanelConstraints.gridy = 0;
        seqPanelConstraints.weightx = 1;
        seqPanelConstraints.gridheight = 1;
        seqPanelConstraints.gridwidth = 1;
        seqPanelConstraints.anchor = GridBagConstraints.WEST;
        seqPanelConstraints.insets = new Insets(5, 15, 5, 15);
        JLabel groupLabel = new JLabel("Group:");
        sequenceInformationPanel.add(groupLabel, seqPanelConstraints);
        
        seqPanelConstraints.gridx = 2;
        seqPanelConstraints.gridy = 0;
        seqPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
        groupSeqText = new JTextField("0000");
        sequenceInformationPanel.add(groupSeqText, seqPanelConstraints);
        
        seqPanelConstraints.gridx = 1;
        seqPanelConstraints.gridy = 1;
        seqPanelConstraints.gridwidth = 2;
        groupCombo = new JComboBox();
        groupCombo.addActionListener(this);
        groupCombo.setMinimumSize(new Dimension(100, 26));
        groupCombo.setPreferredSize(new Dimension(100, 26));
        sequenceInformationPanel.add(groupCombo, seqPanelConstraints);
        
        // Element Column
        seqPanelConstraints.gridx = 3;
        seqPanelConstraints.gridy = 0;
        seqPanelConstraints.gridwidth = 1;
        JLabel elementLabel = new JLabel("Element:");
        sequenceInformationPanel.add(elementLabel, seqPanelConstraints);
        
        seqPanelConstraints.gridx = 4;
        seqPanelConstraints.gridy = 0;
        seqPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
        elementSeqText = new JTextField("0000");
        sequenceInformationPanel.add(elementSeqText, seqPanelConstraints);
        
        seqPanelConstraints.gridx = 3;
        seqPanelConstraints.gridy = 1;
        seqPanelConstraints.gridwidth = 2;
        seqPanelConstraints.fill = GridBagConstraints.NONE;
        elementCombo = new JComboBox();
        elementCombo.addActionListener(this);
        elementCombo.setMinimumSize(new Dimension(100, 26));
        elementCombo.setPreferredSize(new Dimension(100, 26));
        sequenceInformationPanel.add(elementCombo, seqPanelConstraints);
        
        //tag name row
        seqPanelConstraints.gridx = 0;
        seqPanelConstraints.gridy = 2;
        seqPanelConstraints.insets = new Insets(5, 10, 5, 10);
        seqPanelConstraints.anchor = GridBagConstraints.WEST;
        seqPanelConstraints.fill = GridBagConstraints.BOTH;
        seqPanelConstraints.weightx = 0;
        JLabel nameLabel = new JLabel("Tag name:");
        sequenceInformationPanel.add(nameLabel, seqPanelConstraints);
        
        seqPanelConstraints.gridx = 1;
        seqPanelConstraints.gridy = 2;
        seqPanelConstraints.weightx = 1;
        seqPanelConstraints.gridwidth = 3;
        nameValueSeq = new JLabel("Name");
        JScrollPane namePane = new JScrollPane(nameValueSeq);
        namePane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        namePane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        namePane.setBorder(null);
        namePane.setMinimumSize(SCROLL_PANE_SIZE);
        namePane.setPreferredSize(SCROLL_PANE_SIZE);
        namePane.setPreferredSize(SCROLL_PANE_SIZE);
        sequenceInformationPanel.add(namePane, seqPanelConstraints);
        
        //tag value row
        seqPanelConstraints.gridx = 0;
        seqPanelConstraints.gridy = 3;
        seqPanelConstraints.weightx = 0;
        JLabel propertyLabel = new JLabel("Tag value:");
        sequenceInformationPanel.add(propertyLabel, seqPanelConstraints);
        
        seqPanelConstraints.gridx = 1;
        seqPanelConstraints.gridy = 3;
        seqPanelConstraints.weightx = 1;
        seqPanelConstraints.gridwidth = 3;
        propertyValueSeq = new JLabel("Value");
        JScrollPane propPane = new JScrollPane(propertyValueSeq);
        propPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        propPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        propPane.setBorder(null);
        propPane.setMinimumSize(SCROLL_PANE_SIZE);
        propPane.setPreferredSize(SCROLL_PANE_SIZE);
        propPane.setPreferredSize(SCROLL_PANE_SIZE);
        sequenceInformationPanel.add(propPane, seqPanelConstraints);
        
        //button column
        seqPanelConstraints.gridx = 5;
        seqPanelConstraints.gridy = 1;
        seqPanelConstraints.weightx = 0;
        seqPanelConstraints.anchor = GridBagConstraints.CENTER;
        addButtonSeq = new JButton("Add");
        addButtonSeq.setMinimumSize(new Dimension(64, 26));
        addButtonSeq.setPreferredSize(new Dimension(64, 26));
        addButtonSeq.setActionCommand(ADD_TAG_SEQ);
        addButtonSeq.addActionListener(this);
        sequenceInformationPanel.add(addButtonSeq, seqPanelConstraints);
        
        return sequenceInformationPanel;
		
	}
	
	private void buildGroupElementMap(Hashtable<FileDicomKey, FileDicomTag> tagHash) {
		Enumeration<FileDicomKey> e = tagHash.keys();
		while(e.hasMoreElements()) {
			FileDicomKey key = e.nextElement();
			ArrayList<String> allElements = groupToElement.get(key.getGroup());
			if(allElements == null) {
				allElements = new ArrayList<String>(); 
				groupToElement.put(key.getGroup(), allElements);
			}
			if(Integer.valueOf(key.getElement(), 16) != 0) {
				allElements.add(key.getElement());
			}
			String tagName = DicomDictionary.getName(key);
			if(tagName == null || tagName.length() == 0) {
				keyToName.put(key.toString(), PRIVATE);
			} else {
				keyToName.put(key.toString(), tagName);
			}
			
			Object obj = tagHash.get(key).getValue(false);
			if(obj instanceof FileDicomSQ && ((FileDicomSQ)obj).getSequenceLength() > 0) {
				keyToValue.put(key.toString(), SEQUENCE);
			} else if(obj == null || (obj instanceof FileDicomSQ && ((FileDicomSQ)obj).getSequenceLength() == 0)) {
				keyToValue.put(key.toString(), UNKNOWN);
			} else {
				keyToValue.put(key.toString(), obj.toString());
			}
		}
	}
	
	private void buildSeqGroupElementMap(Hashtable<FileDicomKey, FileDicomTag> tagHash) {
		
		//these data structures are filled in only by an open sequence
		this.groupToElementSeq = new TreeMap<String, ArrayList<String>>();
		this.keyToNameSeq = new TreeMap<String, String>();
		this.keyToValueSeq = new TreeMap<String, String>();
		
		Iterator<FileDicomKey> e = tagHash.keySet().iterator();
		
		while(e.hasNext()) {
			FileDicomKey key = e.next();
			ArrayList<String> allElements = groupToElementSeq.get(key.getGroup());
			if(allElements == null) {
				allElements = new ArrayList<String>(); 
				groupToElementSeq.put(key.getGroup(), allElements);
			}
			if(Integer.valueOf(key.getElement(), 16) != 0) {
				allElements.add(key.getElement());
			}
			String tagName = DicomDictionary.getName(key);
			if(tagName == null || tagName.length() == 0) {
				keyToNameSeq.put(key.toString(), PRIVATE);
			} else {
				keyToNameSeq.put(key.toString(), tagName);
			}
			Object obj = tagHash.get(key.toString()).getValue(true);
			if(obj instanceof FileDicomSQ) {
				keyToValueSeq.put(key.toString(), SEQUENCE);
			} else if(obj == null) {
				keyToValueSeq.put(key.toString(), UNKNOWN);
			} else {
				keyToValueSeq.put(key.toString(), obj.toString());
			}
		}
	}
	
	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals(CLEAR_TAGS)) {
			parentPlugin.getTagListTextField().setText("");
		} else if(e.getActionCommand().equals(ADD_TAG)) {
			if(!tagExistsInField(groupList.getSelectedValue()+","+elementList.getSelectedValue())) {
				String existingText = parentPlugin.getTagListTextField().getText();
				String prefix = existingText.length() == 0 || existingText.charAt(existingText.length()-1) == ';' ? "" : ";";
				parentPlugin.getTagListTextField().setText(existingText+prefix+groupList.getSelectedValue()+","+elementList.getSelectedValue()+";");
			}
		} else if(e.getActionCommand().equals(CLOSE)) {
			this.dispose();
		} else if(e.getActionCommand().equals(ADD_TAG_SEQ)) {
			if(!tagExistsInField(groupCombo.getSelectedItem()+","+elementCombo.getSelectedItem())) {
				String existingText = parentPlugin.getTagListTextField().getText();
				String prefix = existingText.length() == 0 || existingText.charAt(existingText.length()-1) == ';' ? "" : ";";
				parentPlugin.getTagListTextField().setText(existingText+prefix+groupCombo.getSelectedItem()+","+elementCombo.getSelectedItem()+";");
			}
		} else if(e.getSource() instanceof JComboBox) {
			if(e.getSource().equals(groupCombo)) {
				Vector<String> v;
				Collections.sort(v = new Vector<String>(groupToElementSeq.get(groupCombo.getSelectedItem())), new NumberComparator());
				elementCombo.setModel(new DefaultComboBoxModel(v));
				elementCombo.setSelectedIndex(0);
			}
			String tagName = groupCombo.getSelectedItem()+","+elementCombo.getSelectedItem();
			nameValueSeq.setText(keyToNameSeq.get(tagName));
			propertyValueSeq.setText(keyToValueSeq.get(tagName));
			sequenceInformationPanel.updateUI();
		} else {
            super.actionPerformed(e);
        }
	}
	
	/**
	 * @return the embeddedPanel
	 */
	public JPanel getEmbeddedPanel() {
		return embeddedPanel;
	}


	private boolean tagExistsInField(String text) {
		String[] tagList = parentPlugin.getTagListTextField().getText().split(";");
		for(int i=0; i<tagList.length; i++) {
			if(tagList[i].equals(text)) {
				return true;
			}
		}
		return false;
	}
	
	public void valueChanged(ListSelectionEvent e) {
		if(e.getSource().equals(groupList.getSelectionModel())) {
			Vector<String> vElement;
	        Collections.sort(vElement = new Vector<String>(groupToElement.get(groupList.getSelectedValue())), new NumberComparator());
	        elementList.setListData(vElement);
	        elementList.setSelectedIndex(0);
	        groupText.getDocument().removeDocumentListener(k1);
	        groupText.setText(groupList.getSelectedValue().toString());
	        groupText.getDocument().addDocumentListener(k1);
	        elementList.updateUI();
		}
		if(groupList.getSelectedIndex() != -1 && elementList.getSelectedIndex() != -1) {
			String tagName = groupList.getSelectedValue().toString()+","+elementList.getSelectedValue().toString();
			nameValue.setText(keyToName.get(tagName));
			String keyValue = keyToValue.get(tagName);
			propPane.setVisible(!keyValue.equals(UNKNOWN));
        	propertyLabel.setVisible(!keyValue.equals(UNKNOWN));
			if(!keyValue.equals(UNKNOWN)) {
				propertyValue.setText(keyValue);
				if(keyValue.equals(SEQUENCE)) {
					add(sequenceInformationPanel, BorderLayout.SOUTH);
					FileDicomSQ sq = (FileDicomSQ)tagList.get(new FileDicomKey(tagName)).getValue(false);
					FileDicomTagTable item = sq.getItem(0);
					buildSeqGroupElementMap(item.getTagList());
					Vector<String> vGroup;
			        Collections.sort(vGroup = new Vector<String>(groupToElementSeq.keySet()), new NumberComparator());
					groupCombo.setModel(new DefaultComboBoxModel(vGroup));
					groupCombo.setSelectedIndex(0);
					Vector<String> vElement;
			        Collections.sort(vElement = new Vector<String>(groupToElementSeq.get(vGroup.get(0))), new NumberComparator());
					elementCombo.setModel(new DefaultComboBoxModel(vElement));
					elementCombo.setSelectedIndex(0);
					pack();
				} else {
					remove(sequenceInformationPanel);
					pack();
				}
			} else {
				remove(sequenceInformationPanel);
				pack();
			}
			elementText.getDocument().removeDocumentListener(k2);
			elementText.setText(elementList.getSelectedValues()[0].toString());
			elementText.getDocument().addDocumentListener(k2);
			nameValue.updateUI();
			tagInformationPanel.updateUI();
		}
	}
	
	private class TagInputListener implements DocumentListener {

		private JTextField updateField;
		private JList updateBox;
		private JDialogDicomTagSelector tagSelector;
		
		public TagInputListener(JTextField updateField, JList updateBox, JDialogDicomTagSelector tagSelector) {
			
			this.updateBox = updateBox;
			this.updateField = updateField;
			this.tagSelector = tagSelector;
			updateField.getDocument().addDocumentListener(this);
		}

		public void changedUpdate(DocumentEvent e) {}

		public void insertUpdate(DocumentEvent e) {
			updateBoxInput();
			
			System.out.println("Insert:" +updateField.getText());
		}

		private void updateBoxInput() {
			if(updateField.getText() != null) {
				try {
					String tag = new String(updateField.getText());
					while(tag.length() < 4) {
						tag += "0";
					}
					tag = tag.substring(0, 4);
					int i=0;
					Integer c1 = Integer.valueOf(tag, 16);
					for(i=0; i<updateBox.getModel().getSize(); i++) {
						Integer c2 = Integer.valueOf(updateBox.getSelectedValues()[0].toString(), 16);
						if(c1 >= c2) {
							break;
						}
					}
					updateBox.getSelectionModel().removeListSelectionListener(tagSelector);
					System.out.println(i);
					updateBox.setSelectedIndex(i);
					updateBox.getSelectionModel().addListSelectionListener(tagSelector);
				} catch(NumberFormatException nfe) {
					
				}
			}
		}

		public void removeUpdate(DocumentEvent e) {
			updateBoxInput();
			System.out.println("Remove:" +updateField.getText());
		}
    }
	
	private class NumberComparator implements Comparator<String>, Serializable {

		public int compare(String o1, String o2) {
			return Integer.valueOf(o1, 16) - Integer.valueOf(o2, 16);
		}
		
	}
}