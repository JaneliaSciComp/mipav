package gov.nih.mipav.model.provenance;

import java.io.IOException;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;


public class ProvenanceEntry extends ModelSerialCloneable {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The timestamp */
    private long time;

    private String action;
    
    private String osName;
    private String osVersion;
    private String user;
    private String mipavVersion;
    private String mipavArguments;
    private String javaVersion;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    public ProvenanceEntry() {}
    
    public ProvenanceEntry(String action, long time) {
       this.action = action;
       this.time = time;
       setToCurrent();
    }

    public void setToCurrent() {
    	osName = System.getProperties().getProperty("os.name");
    	osVersion = System.getProperties().getProperty("os.version");
    	user = System.getProperties().getProperty("user.name");
    	javaVersion = System.getProperties().getProperty("java.version");
    	mipavVersion = MipavUtil.getVersion();
    	mipavArguments = ViewUserInterface.getReference().getCmdLineArguments();
    }
    
    public String toString() {
    	String desc = new String();
    	
    	desc = "action: " + action + ", time (ms): " + time;
    	
    	return desc;
    }
    
    public long getTimeStamp() {
    	return time;
    }
    
    public void setTimeStamp(long ts) {
    	this.time = ts;
    }
    
    public String getUser() {
    	return this.user;
    }
    
    public void setUser(String us) {
    	this.user = us;
    }
    
    public String getAction() {
    	return this.action;
    }
    
    public void setAction(String act) {
    	this.action = act;
    }
    
    public void setOS(String name, String version) {
    	this.osName = name;
    	this.osVersion = version;
    }
    
    public String getOSName() {
    	return this.osName;
    }
    
    public String getOSVersion() {
    	return this.osVersion;
    }
    
    public void setMipavInfo(String version, String arguments) {
    	this.mipavVersion = version;
    	this.mipavArguments = arguments;
    }
    
    public String getMipavVersion() {
    	return this.mipavVersion;
    }
    
    public String getMipavArguments() {
    	return this.mipavArguments;
    }
    
    public String getJavaVersion() {
    	return this.javaVersion;
    }
    
    public void setJavaVersion(String ver) {
    	this.javaVersion = ver;
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
