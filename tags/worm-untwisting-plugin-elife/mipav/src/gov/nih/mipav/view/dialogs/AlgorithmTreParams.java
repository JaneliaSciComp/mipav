package gov.nih.mipav.view.dialogs;

/**
 * This interface contains convenience enumerations for going between the dialog and algorithm of the TRE framework.
 * The enumerations will also be used to simplify the JIST interface to these algorithms.
 * 
 * @author senseneyj
 *
 */

public interface AlgorithmTreParams {
	
	/**
	 * A three way boolean operator to designate the thresholding option used.
	 * 
	 * @author senseneyj
	 *
	 */
	public enum Threshold {
		HARD("Hard Thresholding"),
		SMART("Smart Thresholding"),
		NONE("No Thresholding");
		
		private String str;
		
		private Threshold(String str) {
			this.str = str;
		}
		
		public String toString() {
			return str;
		}
	}
	
	/**
	 * Designates the brand of scanner used for processing.
	 */
	public enum ScannerType {
		PHILIPS,
		GE,
		SIEMENS;
	}
	
	/**
	 * Specifies the magnetic field strength of the scanner used.
	 */
	public enum FieldStrength {
		mri15T("1.5T Field Strength"),
		mri3T("3T Field Strength");
		
		private String str;
		
		private FieldStrength(String str) {
			this.str = str;
		}
		
		public String toString() {
			return str;
		}
	}
	
	/**
	 * Specifies the inversion method used for the scan. 
	 */
	public enum InversionType {
		DOUBLE("Double Inversion Regime"),
		SINGLE("Single Inversion Regime");
		
		private String str;
		
		private InversionType(String str) {
			this.str = str;
		}
		
		public String toString() {
			return str;
		}
	}
}
