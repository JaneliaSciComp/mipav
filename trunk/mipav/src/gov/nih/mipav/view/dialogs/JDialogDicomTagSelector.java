package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.file.DicomDictionary;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomSQ;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagInfo;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.view.MipavUtil;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
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
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;



/**
 * This class allows for DICOM tags to be selected.  Selected DICOM tags can either come from the overall DICOM dictionary or from the tags that exist in
 * a given image.
 * 
 * @author senseneyj
 *
 */
public class JDialogDicomTagSelector extends JDialogScriptableBase implements ListSelectionListener, DicomTagSelectorImpl {

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
	private final Dimension SCROLL_PANE_SIZE = new Dimension(200, 25);

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
	protected JButton addButton, addButtonSeq, clearButton, closeButton;

	/**Name and property labels that describe a DICOM tag for a particular file*/
	private JLabel nameValue, nameValueSeq, propertyValueSeq;

	private JTextField propertyValue;
	
	/**Original list of available DICOM tags*/
	protected Hashtable<FileDicomKey, FileDicomTag> tagList;
	
	/** The parent dialog which receives text from this dicom tag editor */
	private DicomTagSelectorImpl parentDialog;

	/** When this object exists as an embedded panel, this variable contains all necessary data elements. */
	private JPanel embeddedPanel;
	
	/** Panel contains JTable for storing dicom tags. */
	private JPanel tablePanel;
	
	/**When a tag value exists, this label displays the text "Tag value:" */
	private JLabel propertyLabel;

	/**When a tag value exists, this panel displays the value */
	private JScrollPane propPane;

	/** Text fields for displaying/entering a group/element tag */
	private JTextField elementSeqText, groupSeqText, elementText, groupText;

	private TagInputListener k1, k2;

	/** Option table where dicom tags are reported to. */
	protected JTable tagsTable = null;

	protected boolean isStandalone = true;

	private FocusListener addListener;
	
	private JComponent elementFocus;
	
	/** Blank constructor needed for scripting */
	public JDialogDicomTagSelector() {
		super(null, false, false);
	}
	
	public JDialogDicomTagSelector(JDialogBase parent, boolean isStandalone) {
		super(parent, false, false);		
		
		if(parent instanceof DicomTagSelectorImpl) {
			this.parentDialog = (DicomTagSelectorImpl)parent;
		} else {
			System.err.println("No valid tag editor parent was found, events from this dicom tag editor will not be received.");
		}
		
		//these data structures relate to the top level dicom tags
		this.groupToElement = new TreeMap<String, ArrayList<String>>();
		this.keyToName = new TreeMap<String, String>();
		this.keyToValue = new TreeMap<String, String>();
		this.setResizable(true);
		
		this.isStandalone = isStandalone;
		
	}
	
	public void setTagList(Hashtable<FileDicomKey,FileDicomTag> tagList) {
		
		this.tagList = tagList;
		
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
        
        if(parentDialog == null || parentDialog == this) {
        	tablePanel = buildTablePanel();
        }
        
        if(isStandalone) {
        	GridBagLayout layout = new GridBagLayout();
        	GridBagConstraints constraints = new GridBagConstraints();
        	constraints.gridx = 0;
        	constraints.gridy = 0;
        	constraints.weightx = 1;
        	constraints.fill = GridBagConstraints.BOTH;
        	constraints.weighty = .15;
        	setLayout(layout);
	        add(tagSelectorPanel, constraints);
	        constraints.gridy++;
	        constraints.weighty = .05;
	        add(tagInformationPanel, constraints);
	        constraints.gridy++;
	        if(tablePanel != null) {
	        	constraints.weighty = .8;
	        	add(tablePanel, constraints);
	        }
	        
	        pack();
	        
        } else {
        	embeddedPanel = new JPanel();
        	embeddedPanel.add(tagSelectorPanel);
        	embeddedPanel.add(tagInformationPanel);
        	embeddedPanel.add(tablePanel);
        }
        
        addListener = new FocusListener() {
        	public void focusGained(FocusEvent event) {
        		if(event.getSource() == elementText) {
        			elementFocus = elementText;
        			System.out.println("Focus gained: A");
        		} else {
        			elementFocus = elementList;
        			System.out.println("Focus gained: B");
        		}
        	}
        	
        	public void focusLost(FocusEvent event) {
        	}
        };
        
        elementText.addFocusListener(addListener);
        elementList.addFocusListener(addListener);
        elementFocus = elementList;
	}
	
	public class DicomTableModel extends DefaultTableModel {

		public DicomTableModel(String[] names, int i) {
			super(names, i);
		}

		@Override
		public boolean isCellEditable(int row, int column) {
			if(column == 2) {
				return true;
			} else {
				return false;
			}
		}
		
	}
	
	private JPanel buildTablePanel() {
		GridBagLayout tagTableLayout = new GridBagLayout();
        GridBagConstraints tagTableConstraints = new GridBagConstraints();
        tagTableConstraints.anchor = GridBagConstraints.NORTH;

        JPanel tagInformationPanel = new JPanel(tagTableLayout);
        tagInformationPanel.setBorder(MipavUtil.buildTitledBorder("DICOM tags selected"));
        
        //tag name row
        tagTableConstraints.gridx = 0;
        tagTableConstraints.gridy = 0;
        tagTableConstraints.insets = new Insets(5, 10, 5, 10);
        tagTableConstraints.anchor = GridBagConstraints.NORTHWEST;
        tagTableConstraints.fill = GridBagConstraints.BOTH;
        tagTableConstraints.weightx = 1;
        
        String[] names = new String[]{"Tag","Name","Value"};
        DefaultTableModel model = new DicomTableModel(names, 0);
        
        tagsTable = new JTable(model);
        tagsTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        tagsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        tagsTable.getColumn("Tag").setMinWidth(90);
        tagsTable.getColumn("Tag").setMaxWidth(90);
        //tagsTable.getColumn("Tag").setCellRenderer(new TagCodeRenderer());
        tagsTable.getColumn("Name").setMinWidth(160);
        tagsTable.getColumn("Name").setMaxWidth(500);
        //tagsTable.getColumn("Name").setCellRenderer(new TagReferenceRenderer());
        tagsTable.getColumn("Value").setMinWidth(50);
        tagsTable.getColumn("Value").setMaxWidth(1000);
        //tagsTable.getColumn("Value").setCellRenderer(new TagReferenceRenderer());

        //tagsTable.getTableHeader().addMouseListener(new HeaderListener());
        tagsTable.setSelectionBackground(new Color(184, 230, 255));
        
        tagsTable.setEditingRow(2);
        tagsTable.setMinimumSize(new Dimension(90, 488));
        tagsTable.setPreferredSize(new Dimension(90, 488));
        
        tagsTable.addKeyListener(new KeyListener() {

			@Override
			public void keyPressed(KeyEvent arg0) {
				switch(arg0.getKeyCode()) {
				
				case KeyEvent.VK_DELETE:
				case KeyEvent.VK_BACK_SPACE:
					if(parentDialog.getTagTable() != null && parentDialog.getTagTable().getSelectedRows() != null) {
						DefaultTableModel model = (DefaultTableModel)parentDialog.getTagTable().getModel();
						int[] rowSelect = parentDialog.getTagTable().getSelectedRows();
						for(int i=rowSelect.length-1; i>=0; i--) {
							model.removeRow(rowSelect[i]);
						}
					}
					
				}
			}

			@Override
			public void keyReleased(KeyEvent arg0) {}

			@Override
			public void keyTyped(KeyEvent arg0) {}
        	
        });
        
        JScrollPane scroll = new JScrollPane(tagsTable);
        scroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scroll.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
        scroll.setMinimumSize(new Dimension(94, 508));
        scroll.setPreferredSize(new Dimension(94, 508));
        
        tagInformationPanel.add(scroll, tagTableConstraints);
        
        return tagInformationPanel;
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
        if(groupList.getModel().getSize() > 0) {
        	groupText.setText(groupList.getModel().getElementAt(0).toString());
        }
        
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
        Vector<String> vElement = new Vector<String>();
        if(vGroup.size() > 0) {
        	Collections.sort(vElement = new Vector<String>(groupToElement.get(vGroup.get(0))), new NumberComparator());
        }
        elementList = new JList(vElement);
        if(elementList.getModel().getSize() > 0) {
        	elementList.setSelectedIndex(0);
        }
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
        if(elementList.getModel().getSize() > 0) {
        	elementText.setText(elementList.getModel().getElementAt(0).toString());
        }
        
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
        String tagName = new String();
        if(groupList.getSelectedValue() != null && elementList.getSelectedValue() != null) {
        	tagName = groupList.getSelectedValue().toString()+","+elementList.getSelectedValue().toString();
        }
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
        propertyValue = new JTextField(keyToValue.get(tagName));
        propPane = new JScrollPane(propertyValue);
        propPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        propPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        propPane.setMinimumSize(SCROLL_PANE_SIZE);
        propPane.setMaximumSize(SCROLL_PANE_SIZE);
        propPane.setPreferredSize(SCROLL_PANE_SIZE);
        propPane.setBorder(null);
        tagInformationPanel.add(propPane, infoPanelConstraints);
        
        if(keyToValue.get(tagName) == null || keyToValue.get(tagName).equals(UNKNOWN)) {
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
				Object objectVal = tagHash.get(key).getValue(true);
				String objectString = new String();
				if(objectVal == null) {
					//use empty object string for MIPAV-required dicom tags that don't exist in image
				} else {
					objectString = objectVal.toString();
				}
				keyToValue.put(key.toString(), objectString);
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
			if(parentDialog.getTagListTextField() != null) {
				parentDialog.getTagListTextField().setText("");
			} else {
				DefaultTableModel model = (DefaultTableModel)parentDialog.getTagTable().getModel();
				int[] rowSelect = parentDialog.getTagTable().getSelectedRows();
				for(int i=rowSelect.length-1; i>=0; i--) {
					model.removeRow(rowSelect[i]);
				}
			}
			
		} else if(e.getActionCommand().equals(ADD_TAG)) {
			
			String tag = new String();
			if(elementFocus == elementList) {
				tag = groupList.getSelectedValue()+","+elementList.getSelectedValue();
			} else {
				tag = groupText.getText()+","+elementText.getText();
			}
			if(!tagExistsInField(tag)) {
				if(parentDialog.getTagListTextField() != null) {
					String existingText = parentDialog.getTagListTextField().getText();
					String prefix = existingText.length() == 0 || existingText.charAt(existingText.length()-1) == ';' ? "" : ";";
					parentDialog.getTagListTextField().setText(existingText+prefix+tag+";");
				} else {
					DefaultTableModel model = (DefaultTableModel)parentDialog.getTagTable().getModel();
					//FileDicomTag tagValue = tagList.get(new FileDicomKey(tag));
					model.addRow(new String[]{
							tag,
							nameValue.getText(),
							propertyValue.getText()
					});
				}
			} 
		} else if(e.getActionCommand().equals(CLOSE)) {
			this.dispose();
		} else if(e.getActionCommand().equals(ADD_TAG_SEQ)) {
			if(!tagExistsInField(groupCombo.getSelectedItem()+","+elementCombo.getSelectedItem())) {
				String existingText = parentDialog.getTagListTextField().getText();
				String prefix = existingText.length() == 0 || existingText.charAt(existingText.length()-1) == ';' ? "" : ";";
				parentDialog.getTagListTextField().setText(existingText+prefix+groupCombo.getSelectedItem()+","+elementCombo.getSelectedItem()+";");
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
		if(parentDialog.getTagListTextField() != null) {
			String[] tagList = parentDialog.getTagListTextField().getText().split(";");
			for(int i=0; i<tagList.length; i++) {
				if(tagList[i].equals(text)) {
					return true;
				}
			}
			return false;
		} else {
			TableModel model = parentDialog.getTagTable().getModel();
			for(int i=1; i<model.getRowCount(); i++) {
				if(model.getValueAt(i, 0) != null && model.getValueAt(i, 0).toString().equals(text)) {
					return true;
				}
			}
			return false;
		}
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
			if(keyValue != null) {
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
			String updateText = updateField.getText();
			if(updateText.length() == 4) {
				String groupStr = groupText.getText();
				if(groupStr.length() == 4) {
					try {
						int group = Integer.valueOf(groupStr, 16);
						int element = Integer.valueOf(updateText, 16);
						FileDicomTagInfo tagInfo = DicomDictionary.getDicomTagTable().get(new FileDicomKey(group, element));
						nameValue.setText(tagInfo.getName());
					} catch(Exception ex) {
						
					} catch(Error er) {
						
					}
				}
			}
			
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

	@Override
	public JTextField getTagListTextField() {
		//This class uses the newer table method for selecting dicom tags
		return null;
	}


	@Override
	public JTable getTagTable() {
		return tagsTable;
	}


	public DicomTagSelectorImpl getParentDialog() {
		return parentDialog;
	}


	public void setParentDialog(DicomTagSelectorImpl parentDialog) {
		this.parentDialog = parentDialog;
	}


	@Override
	protected void callAlgorithm() {
		return;
	}


	@Override
	protected void setGUIFromParams() {
		return;
	}


	@Override
	protected void storeParamsFromGUI() throws ParserException {
		return;
	}
}