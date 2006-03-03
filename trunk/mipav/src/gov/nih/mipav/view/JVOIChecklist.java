package gov.nih.mipav.view;


import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;

/**
*
*		@version    1.0 Mar 1, 1999
*		@author     Matthew J. McAuliffe, Ph.D.
*
*/

public class JVOIChecklist extends JPanel implements MouseListener{
    JList   list;

    public JVOIChecklist (ViewList lists[]) {

        int     i;
        Font    serif;
        Icon    icon1;
        Icon    icon2;
        Vector  data;

        serif  = MipavUtil.font12;
        setFont(serif);

        //setBackground(Color.lightGray);
        if (lists == null) return;

        list = new JList();
        list.setFont(serif);
        list.setBackground(Color.lightGray);
        list.setCellRenderer(new ListItemRenderer());

        data = new Vector();

        icon1 = MipavUtil.getIcon("boxs.gif");
        icon2 = MipavUtil.getIcon("boxu.gif");

        for (i = 0; i < lists.length; i++) {
            data.addElement(new ListItem(lists[i], icon1, icon2 ));
        }

        list.setListData(data);
        list.addMouseListener(this);
        list.setSelectedIndex(1);

        add(list, "Center");
        validate();
    }


    public void updateList(ViewList lists[]) {
        ListItem item;

        for (int i = 0; i < lists.length; i++) {
            item = (ListItem)(list.getModel().getElementAt(i));

            if (lists[i].getState() == true) {
                if(!item.isSelected()) item.toggleSelected();
            }
            else {
                if(item.isSelected()) item.toggleSelected();
            }
        }
        repaint();
    }

    public void mouseClicked(MouseEvent event){
    }
    public void mousePressed(MouseEvent event){
    }
    public void mouseReleased(MouseEvent event){
        int index = list.getSelectedIndex();
        ListItem item = (ListItem)(list.getModel().getElementAt(index));
        item.toggleSelected();
        repaint();
    }
    public void mouseEntered(MouseEvent event){
    }
    public void mouseExited(MouseEvent event){
    }
}




class ListItem {

    private Icon        iconSelected;
    private Icon        iconUnselected;
    private ViewList    list;

    public ListItem(ViewList _list, Icon iconS, Icon iconU){

        list            = _list;
        iconSelected    = iconS;
        iconUnselected  = iconU;
    }

    public Icon getIcon() {

        if (list.getState() == true)  {
            return iconSelected;
        }
        else  {
            return iconUnselected;
        }
    }

    public String getTitle(){
        return list.getString();
    }

    public void toggleSelected() {

        if (list.getState() == true) {
            list.setState(false);
        }
        else  {
            list.setState(true);
        }
    }

    public boolean isSelected() {
        return list.getState();
    }

}

class ListItemRenderer implements ListCellRenderer {

    JLabel label;

    public ListItemRenderer() {

        label = new JLabel();
        label.setOpaque(true);
    }


    public Component getListCellRendererComponent ( JList    list,
                                                    Object   value,
                                                    int      index,
                                                    boolean  isSelected,
                                                    boolean  cellHasFocus) {
        ListItem listItem = null;

        if (value instanceof ListItem ) {
            listItem = (ListItem) value;
        }

        label .setFont(list.getFont());

        if (isSelected) {
            label.setBackground(list.getSelectionBackground());
            label.setForeground(list.getSelectionForeground());
        }
        else {
            label.setBackground(list.getBackground());
            label.setForeground(list.getForeground());
        }

        if (listItem != null) {
            label.setText(listItem.getTitle());
            label.setIcon(listItem.getIcon());
        }
        else  {
            label.setText(value.toString());
        }

        return label;
    }
}
