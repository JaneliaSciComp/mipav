package gov.nih.mipav.model.provenance;

import java.util.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;



public class ProvenanceEntry extends ModelSerialCloneable {

    //~ Instance fields ------------------------------------------------------------------------------------------------

	private String programName;
	
    /** The timestamp */
    private String timeStamp;

    /** action describing the event */
    private String action;
    
    /** the name of the operating system */
    private String platform;
    
    /** the operating system's version*/
    private String platformVersion;
    
    /** the user name*/
    private String user;
    
    /** the machine host name */
    private String hostName;
    
    /** mipav's version */
    private String mipavVersion;

    /** probably unused */
    private String mipavBuild;
    
    /** mipav cmd line arguments */
    private String mipavInputs;
    
    /** probably unused */
    private String mipavOutputs;
    
    /** java's version */
    private String compilerVersion;
    
    private String architecture;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    public ProvenanceEntry() {}
    
    /**
     * Constructor created when an action happens
     * @param action the action performed
     */
    public ProvenanceEntry(String action) {
       this.action = action;
       setToCurrent();
    }

    /**
     * Sets the variables to the current system
     *
     */
    public void setToCurrent() {
    	Calendar cal = Calendar.getInstance();
    	//System.err.println(cal);
    	timeStamp = cal.get(Calendar.YEAR) + "/" + (cal.get(Calendar.MONTH) + 1) + "/" 
    		+ cal.get(Calendar.DAY_OF_MONTH) + "-" + cal.get(Calendar.HOUR_OF_DAY) + "-" 
    		+ cal.get(Calendar.MINUTE) + "-" + cal.get(Calendar.SECOND) + "-"
    		+ cal.get(Calendar.MILLISECOND) + "-" + cal.getTimeZone().getDisplayName();
    	//System.err.println("timestamp: " + timeStamp);
    	platform = System.getProperties().getProperty("os.name");
    	platformVersion = System.getProperties().getProperty("os.version");
    	architecture = System.getProperties().getProperty("os.arch");
    	user = System.getProperties().getProperty("user.name");
    	compilerVersion = System.getProperties().getProperty("java.version");
    	
    	mipavVersion = MipavUtil.getVersion();
    	programName = "MIPAV";
    	mipavInputs = ViewUserInterface.getReference().getCmdLineArguments();
    	
    	try
    	{
    		java.net.InetAddress localMachine = java.net.InetAddress.getLocalHost();	
    		hostName = localMachine.getHostName();
    	}
    	catch(java.net.UnknownHostException uhe)
    	{
    		hostName = "";
    		//handle exception
    	}
    	
    }
    
    /**
     * prints out a few things (not done)
     */
    public String toString() {
    	String desc = new String();
    	
    	desc = "action: " + action + ", time: " + timeStamp;
    	
    	return desc;
    }
    
    public void setArchitecture(String arch) {
    	this.architecture = arch;
    }
    
    public String getArchitecture() {
    	return this.architecture;
    }
    
    public void setProgram(String version, String build) {
    	this.mipavVersion = version;
    	this.mipavBuild = build;
    }
    
    public String getProgramName() {
    	return this.programName;
    }
    
    public void setProgramName(String pn) {
    	this.programName = pn;
    }
    
    public void setProgramArguments(String inputs, String outputs) {
    	this.mipavInputs = inputs;
    	this.mipavOutputs = outputs;
    }
    
    public String getProgramInputs() {
    	return this.mipavInputs;
    }
    
    public String getProgramOutputs() {
    	return this.mipavOutputs;
    }
    
    public String getTimeStamp() {
    	return timeStamp;
    }
    
    public void setTimeStamp(String ts) {
    	this.timeStamp = ts;
    }
    
    public String getUser() {
    	return this.user;
    }
    
    public void setUser(String us) {
    	this.user = us;
    }
    
    public void setHostName(String hn) {
    	this.hostName = hn;
    }
    
    public String getHostName() {
    	return this.hostName;
    }
    
    public String getAction() {
    	return this.action;
    }
    
    public void setAction(String act) {
    	this.action = act;
    }
    
    public void setPlatform(String name) {
    	this.platform = name;
    }
    
        
    public String getPlatform() {
    	return this.platform;
    }
    
    public void setPlatformVersion(String pv) {
    	this.platformVersion = pv;
    }
    
    public String getPlatformVersion() {
    	return this.platformVersion;
    }
    
    public String getMipavVersion() {
    	return this.mipavVersion;
    }
    
    public String getJavaVersion() {
    	return this.compilerVersion;
    }
    
    public void setJavaVersion(String ver) {
    	this.compilerVersion = ver;
    }
    
    /**
     * Copies the object.
     *
     * @return  Returns the cloned object.
     */
    public Object clone() {
        Object obj = super.clone();
       
        return (obj);

    }
    
}
