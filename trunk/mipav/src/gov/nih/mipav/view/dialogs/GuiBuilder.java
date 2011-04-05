package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogTreT1.ExitStatus;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

/**
 * Provides methods for quickly building panel components. I can think of many other (better)
 * ways to do this, but for the ImageJ port this works well for now.
 * 
 * @author senseneyj
 *
 */
public class GuiBuilder implements ActionListener {

    public static final int GUI_BUILDER_OK_ID = ActionEvent.RESERVED_ID_MAX + 20;

    private ArrayList<ActionListener> listenerList;
    
    private boolean passedListeners;

    private ExitStatus exit;
    
    private JDialogBase parent;
    
    public GuiBuilder(JDialogBase parent) {
        this.parent = parent;
        this.listenerList = new ArrayList<ActionListener>();
        this.exit = ExitStatus.INCOMPLETE;
    }

    public ExitStatus getExitStatus() {
        return exit;
    }

    public ActionListener[] getListenerList() {
        ActionListener[] list = new ActionListener[listenerList.size()];
        for(int i=0; i<listenerList.size(); i++) {
            list[i] = listenerList.get(i);
        }
        return list;
    }

    public JRadioButton buildRadioButton(String label, boolean selected) {
        FlowLayout f = new FlowLayout();
        f.setAlignment(FlowLayout.LEFT);
        JPanel radioPanel = new JPanel(f);
        JRadioButton radioButton = new JRadioButton(label);
        radioButton.setSelected(selected);
        radioPanel.add(radioButton);
        return radioButton;
    }

    public JCheckBox buildCheckBox(String label, boolean selected) {
        FlowLayout f = new FlowLayout();
        f.setAlignment(FlowLayout.LEFT);
        JPanel checkPanel = new JPanel(f);
        JCheckBox checkBox = new JCheckBox(label);
        checkBox.setSelected(selected);
        checkPanel.add(checkBox);
        return checkBox;
    }
    
    public JTextField buildField(String labelText, String initText) {
        FlowLayout f = new FlowLayout();
        f.setAlignment(FlowLayout.LEFT);
        JPanel panel = new JPanel(f);
        JLabel label = new JLabel(labelText);
        JTextField text = new JTextField(initText);
        text.setColumns(8);
        panel.add(label);
        panel.add(text);
        return text;
    }
    
    public JTextField buildFileField(String labelText, String initText, final boolean multiSelect, final int fileSelectionMode) {
        FlowLayout f = new FlowLayout();
        f.setAlignment(FlowLayout.LEFT);
        JPanel panel = new JPanel(f);
        JLabel label = new JLabel(labelText);
        final JTextField text = new JTextField(initText);
        text.setColumns(8);
        JButton button = new JButton("Browse");
        ActionListener listener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JFileChooser fileChooser = new JFileChooser(Preferences.getImageDirectory());
                fileChooser.setFont(MipavUtil.defaultMenuFont);
                fileChooser.setMultiSelectionEnabled(multiSelect);
                fileChooser.setFileSelectionMode(fileSelectionMode);
                
                Dimension d = new Dimension(700, 400);
                fileChooser.setMinimumSize(d);
                fileChooser.setPreferredSize(d);
                
                int returnVal = fileChooser.showOpenDialog(null);
                            
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    File selectedFile = fileChooser.getSelectedFile();
                    Preferences.setImageDirectory(fileChooser.getCurrentDirectory());
                    if(!selectedFile.exists() || !selectedFile.canRead()) {
                        MipavUtil.displayError(selectedFile.getName() + " could not be found.");
                        return;
                    }
                    
                    text.setText(selectedFile.toString());
                    text.updateUI();
                }
            }
        };
        button.addActionListener(listener);
        
        ActionListener textListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(e.getSource().equals(parent.OKButton)) {
                    File f = new File(text.getText());
                    if(!f.exists() || !f.canRead()) {
                        passedListeners = false;
                    }
                }
            }
        };
        
        listenerList.add(textListener);
        panel.add(label);
        panel.add(text);
        panel.add(button);
        
        return text;
        
    }
    
    public JTextField buildIntegerField(final String labelText, int initNum) {
        final JTextField genericField = buildField(labelText, String.valueOf(initNum));
        ActionListener listener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(e.getSource().equals(parent.OKButton)) {
                    try {
                        Integer.valueOf(genericField.getText());
                    } catch(NumberFormatException e1) {
                        try {
                            double d = Double.valueOf(genericField.getText());
                            if(((int)d) == d) {
                                genericField.setText(Integer.valueOf((int)d).toString());
                                return;
                            } else {
                                MipavUtil.displayInfo(labelText+" must be an integer.");
                                passedListeners = false;
                            }
                        } catch(NumberFormatException e2) {
                            MipavUtil.displayInfo(labelText+" must be an integer.");
                            passedListeners = false;
                        }
                    }
                }
            }
        };
        genericField.addActionListener(listener);
        listenerList.add(listener);
        return genericField;
    }
    
    public JTextField buildDecimalField(final String labelText, double initNum) {
        final JTextField genericField = buildField(labelText, String.valueOf(initNum));
        ActionListener listener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(e.getSource().equals(parent.OKButton)) {
                    try {
                        Double.valueOf(genericField.getText());
                    } catch(NumberFormatException e1) {
                        MipavUtil.displayInfo(labelText+" must be a number.");
                        passedListeners = false;
                    }
                }
            }
        };
        genericField.addActionListener(listener);
        listenerList.add(listener);
        return genericField;
    }
    
    public JComboBox buildComboBox(String labelText, Object[] options) {
        FlowLayout f = new FlowLayout();
        f.setAlignment(FlowLayout.LEFT);
        JPanel panel = new JPanel(f);
        JLabel label = new JLabel(labelText);
        JComboBox comboBox = null;
        if(options != null) {
            
            comboBox = new JComboBox(options);
        } else {
            comboBox = new JComboBox(new String[]{"a", "B"});
        }
     
        panel.add(label);
        panel.add(comboBox);
        return comboBox;
    }
    
    public JComboBox buildComboBox(String labelText, Object[] options, int numDefault) {
        JComboBox comboBox = buildComboBox(labelText, options); //call default
        if(numDefault > comboBox.getItemCount()-1) {
            numDefault = 0;
        } else {
            comboBox.setSelectedIndex(numDefault);
        }
        //TODO: get renderer to truncate long names
        /*comboBox.setRenderer(new ListCellRenderer() {
            DefaultListCellRenderer defaultRenderer = new DefaultListCellRenderer();
            
            public Component getListCellRendererComponent(JList list,
                    Object value, int index, boolean isSelected,
                    boolean cellHasFocus) {
                JLabel renderer = (JLabel) defaultRenderer.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if(index == -1 && value.toString().length() > 23) {
                    renderer.setText(value.toString().substring(0, 23)+"...");
                } else {
                    //renderer.setBounds(0, 0, 300, 20);
                }
                System.out.println(value+" "+index);
                return renderer;
            
            }   
        });*/
        return comboBox;
    }
    
    public JPanel buildOKCancelPanel() {
        JPanel panel = new JPanel();
        parent.OKButton = new JButton("OK");
        parent.OKButton.addActionListener(this);
        parent.OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
        parent.OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
        parent.OKButton.setFont(MipavUtil.font12B);
        
        parent.cancelButton = new JButton("Cancel");
        parent.cancelButton.addActionListener(this);
        parent.cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
        parent.cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
        parent.cancelButton.setFont(MipavUtil.font12B);
        
        panel.add(parent.OKButton);
        panel.add(parent.cancelButton);
        return panel;
    }

    public void actionPerformed(ActionEvent e) {
        passedListeners = true;
        if(e.getSource().equals(parent.OKButton)) {
            for(int i=0; i<listenerList.size(); i++) {
                if(passedListeners) {
                    listenerList.get(i).actionPerformed(e);
                } else {
                    exit = ExitStatus.OK_FAIL;
                    return;
                }
            }
            if(passedListeners) {
                exit = ExitStatus.OK_SUCCESS;
                parent.actionPerformed(e);
            } else {    
                exit = ExitStatus.OK_FAIL;
                return;
            }
        } else if(e.getSource().equals(parent.cancelButton)) {
            exit = ExitStatus.CANCEL;
            parent.dispose();
        }
    }
}