package gov.nih.mipav.model.structures;

/**
*
*    This class is can be thrown by any class that wishes to indicate
*    an error while "newing" memory space for an object.
*
*		@version 0.1 Nov 1, 1997
*		@author Matthew J. McAuliffe
*
*/


public class ExceptionAllocateData extends Exception {

	/**
	* Default constructor
	*/
	public ExceptionAllocateData() {
		super("ExceptionAllocateData");
    }

	/**
	* Constructor for specific data
	*/
	public ExceptionAllocateData(String str) {
		super(str);
    }
}