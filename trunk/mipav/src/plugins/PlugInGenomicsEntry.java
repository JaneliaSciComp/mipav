
public class PlugInGenomicsEntry {
	public Object[] rowData;
	
	public boolean selected;
	public boolean required;
	public String type;
	
	public PlugInGenomicsEntry(Object[] pSubject){
		selected = false;
		rowData = new Object[pSubject.length + 1];
		rowData[0] = selected;
		for(int i=0; i<pSubject.length; i++) {
			// put in check for enum and add JComboBox below instead of direct assignment
			rowData[i+1] = pSubject[i];
		}
	}
	
	public Object getValueAt(int col){
		return rowData[col];
	}
	
	public void setValueAt(Object value, int col) {
		if (col == 0) {
			selected = (Boolean)value;
		}
		rowData[col] = value;
	}
	
	public boolean isRequired(){
		return required;
	}
	
	public int getRowSize(){
		return rowData.length;
	}
	
	public Object[] getRowData() {
		return rowData;
	}
	
	public Object getValue(int indx){
		return rowData[indx]; 
	}
	
	public boolean isRowSelected() {
		return selected;
	}
	
	@Override
	public String toString() {
		return "Row: " + rowData.toString();
	}
}
