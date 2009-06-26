package gov.nih.mipav.view.xcede;

import java.util.EventListener;
import java.util.Vector;
import java.util.Enumeration;
import javax.swing.event.TableModelListener;
import javax.swing.table.TableModel;
import javax.swing.event.EventListenerList;
import javax.swing.event.TableModelEvent;
import gov.nih.mipav.model.file.xcede.*;

public class JXCEDETableModel implements TableModel {
    private transient Vector entryList;
    private XCEDEElement rootElement;
    private EventListenerList listenerList;
    
    public JXCEDETableModel(XCEDEElement root){
        this.rootElement = root;
        entryList = new Vector();
        listenerList = new EventListenerList();
        Enumeration keys = rootElement.keys();
        while(keys.hasMoreElements()){
            String key = (String)keys.nextElement();
            if(key.equals(Element.LEVEL)){
                continue;
            }else if(key.equals(Element.PARENT_ELEMENT)){
                continue;
            }else if(key.equals(XCEDEElement.XCEDE_DATAREC_IMAGEFRAME)){
                continue;
            }else if(rootElement.isChild(key)){
                continue;
            }else if(rootElement.get(key) instanceof Vector){
                Vector elementList = (Vector)rootElement.get(key);
                for(int i = 0; i < elementList.size(); i++){
                    entryList.add(new Entry(key, elementList.get(i)));
                }
            }else{
                entryList.add(new Entry(key, rootElement.get(key)));
            }
        }
        
    }
    public int getRowCount() {
        return entryList.size();
    }

    public int getColumnCount() {
        return 2;
    }

    public String getColumnName(int columnIndex) {
        if(columnIndex == 0){
            return "Key";
        }else if(columnIndex == 1){
            return "Value";
        }
        return "";
    }

    public Class getColumnClass(int columnIndex) {
        return String.class;
    }

    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return false;
    }

    public Object getValueAt(int rowIndex, int columnIndex) {
        if(columnIndex == 0){
            return ((Entry)entryList.get(rowIndex)).getKey();
        }else if(columnIndex == 1){
            return ((Entry)entryList.get(rowIndex)).getValue().toString();
        }
        return null;
    }

    public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
        // TODO Auto-generated method stub

    }

    public void addTableModelListener(TableModelListener l) {
        listenerList.add(TableModelListener.class, l);
    }

    public void removeTableModelListener(TableModelListener l) {
        listenerList.remove(TableModelListener.class, l);
    }

    /**
     * Notifies all the listeners taht the table was changed.
     * @param oldRoot the root node of the tree.
     */
    public void fireTableChanged(TableModelEvent e){
        EventListener[] listeners = listenerList.getListeners(TableModelListener.class);
        for(int i = 0; i < listeners.length; i++){
            ((TableModelListener)listeners[i]).tableChanged(e);
        }
    }
    
    /**
     * A helper class to store the key and value pair information.
     */
    private class Entry{
        private Object key;
        private Object value;
        
        public Entry(){
            this(null, null);
        }
        
        public Entry(Object key, Object value){
            this.key = key;
            this.value = value;
        }
        
        public Object getKey(){
            return key;
        }
        
        public void setKey(Object key){
            this.key = key;
        }
        public Object getValue(){
            return value;
        }
        
        public void setValue(Object value){
            this.value = value;
        }
    }
}
