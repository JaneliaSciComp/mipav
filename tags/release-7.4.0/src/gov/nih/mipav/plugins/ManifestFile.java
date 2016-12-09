package gov.nih.mipav.plugins;

import gov.nih.mipav.view.MipavUtil;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

/**
 * Stores the default manifest file as a static reference, or optionally can be used for creating custom manifest files.
 * A local and system copy of the manifest file are stored.  By storing manifest entries as class files, this should remove
 * any classpath problems occurring for plugins using the install/uninstall feature.
 * 
 * @author senseneyj
 */
public class ManifestFile {
	
	// ~ Static fields/initializers-----------------------------------------------------------
	
	public static final String MANIFEST_LOC = System.getProperty("user.home") + File.separator + "mipav" + 
												File.separator + "plugins" + File.separator + "MANIFEST" + File.separator;

	/** Signifies manifest file lists a plugin after delimiter. */
	private static final String PLUGIN_ENTRY = "plugin-name";
	
	/** Signifies manifest file lists a dependent after delimiter. */
	private static final String DEPENDENT_ENTRY = "dependent-name";

	/** Standard delimiter after manifest file. */
	private static final String DELIMITER = ":\t";
	
	/**System-dependent line delimiter. */
	private static final String LINE_DELIM = System.getProperty("line.separator");
	
	//~ Instance fields ----------------------------------------------------------------------
	
	private File manifest;
	
	private long timeLoaded;
	
	private static ManifestFile staticRef = new ManifestFile();
	
	/** The manifest file as a set of class files. */
	private HashMap<Class, ArrayList<Class>> manifestInfo;
	
	//~ Constructors -------------------------------------------------------------------------
	
	/**
	 * Gets/creates manifest file in default location.
	 */
	public ManifestFile() {
		manifest = new File(MANIFEST_LOC+"plugins.mf");
	
		init();
	}
	
	/**
	 * @param alternateLoc where the manifest file is/should be created
	 */
	public ManifestFile(String alternateLoc) {
		this.manifest = new File(alternateLoc);
	
		init();
	}
	
	/**
	 * Gets the default manifest, guaranteed to be latest default version unless working in threaded environment.
	 */
	public static ManifestFile getReference() {
		if(staticRef.manifest.lastModified() > staticRef.timeLoaded) {
			staticRef.refresh();
		}
		
		return staticRef;
	}
	
	//~ Methods ------------------------------------------------------------------------------
	
	/**
	 * Rereads the manifest to load any changes, whether or not any changes have been made.
	 */
	public void refresh() {
		
		manifestInfo = parseManifestInfo();
		
		timeLoaded = System.currentTimeMillis();
	}
	
	private void init() {
		if(!manifest.exists()) {
			createManifestFile();
			manifestInfo = new HashMap<Class, ArrayList<Class>>();
		} else {
			manifestInfo = parseManifestInfo();
		}
		
		timeLoaded = System.currentTimeMillis();
	}
	
	private HashMap<Class, ArrayList<Class>> parseManifestInfo() {
		HashMap<Class, ArrayList<Class>> manifestInfo = new HashMap<Class, ArrayList<Class>>();
		
		BufferedReader in = null;
		try {
			in = new BufferedReader(new FileReader(manifest));
		
			if(in != null) {
				String nextLine = new String();
				boolean inPlugin = false;
				int beginIndex = 0;
				ArrayList<Class> currentPlugin = new ArrayList<Class>();
				Class pluginClass = null, dependentClass = null;
				while((nextLine = in.readLine()) != null) {
					if(!inPlugin && (beginIndex = nextLine.indexOf(PLUGIN_ENTRY+DELIMITER)) != -1) { //line-delim has been stripped
						try {
							pluginClass = Class.forName(nextLine.substring(beginIndex+PLUGIN_ENTRY.length() + DELIMITER.length()));
							inPlugin = true;
						} catch (ClassNotFoundException e) {
							MipavUtil.displayInfo(nextLine+" could not be read as a class. Please check your class path");
							e.printStackTrace();
						}
					} else if(inPlugin) {
						if(nextLine.contains(PLUGIN_ENTRY)) {
							manifestInfo.put(pluginClass, currentPlugin);
							pluginClass = null;
							currentPlugin = new ArrayList<Class>();
							inPlugin = false;
							
							if((beginIndex = nextLine.indexOf(PLUGIN_ENTRY+DELIMITER)) != -1) { //line-delim has been stripped
								try {
									pluginClass = Class.forName(nextLine.substring(beginIndex+PLUGIN_ENTRY.length() + DELIMITER.length()));
									inPlugin = true;
								} catch (ClassNotFoundException e) {
									MipavUtil.displayInfo(nextLine+" could not be read as a class. Please check your class path");
									e.printStackTrace();
								}
							} 
						} else if (nextLine.contains(DEPENDENT_ENTRY)){
							beginIndex = nextLine.indexOf(DEPENDENT_ENTRY);
							try {
								dependentClass = Class.forName(nextLine.substring(beginIndex + DEPENDENT_ENTRY.length() + DELIMITER.length()));
								currentPlugin.add(dependentClass);
							} catch (ClassNotFoundException e) {
								MipavUtil.displayInfo(nextLine+" could not be read as a class. Please check your class path");
								e.printStackTrace();
							}
						}
					} 
				}
				
				if(pluginClass != null && manifestInfo.get(pluginClass) == null) {
					manifestInfo.put(pluginClass, currentPlugin);
				}
				
				in.close();
			}
		
		} catch (FileNotFoundException e) {
			MipavUtil.displayInfo("Unable to locate manifest file.");
			e.printStackTrace();
		} catch (IOException e) {
			MipavUtil.displayInfo("Unable to write to manifest file.");
			e.printStackTrace();
		}	
		
		return manifestInfo;
	}
	
	/**
	 * Adds an entry to the manifest file, does in fact write to the file.  If the entry errantly exists,
	 * this method modifies the entry.
	 * 
	 * @param c the plugin class
	 * @param dependents the classes that a given plugin uses
	 */
	public void addEntry(Class c, Class... dependents) {
		if(manifestInfo.get(c) != null) {
			modifyEntry(c, dependents);
		}
		
		ArrayList<Class> pluginDep = new ArrayList<Class>();
		for(int i=0; i<dependents.length; i++) {
			pluginDep.add(dependents[i]);
		}
		manifestInfo.put(c, pluginDep);
		
		addToFile(c, pluginDep);
		
	}
	
	private void addToFile(Class c, ArrayList<Class> dependents) {
		BufferedWriter out = null;
		try {
			out = new BufferedWriter(new FileWriter(manifest, true));
		
			if(out != null) {

				out.write(PLUGIN_ENTRY+DELIMITER+c.getName()+LINE_DELIM);
				for(int i=0; i<dependents.size(); i++) {	
					out.write(DEPENDENT_ENTRY+DELIMITER+dependents.get(i).getName()+LINE_DELIM);
				}
				out.write(LINE_DELIM);
				
				out.flush();
				out.close();
			}
		
		} catch (FileNotFoundException e) {
			MipavUtil.displayInfo("Unable to locate manifest file.");
			e.printStackTrace();
		} catch (IOException e) {
			MipavUtil.displayInfo("Unable to write to manifest file.");
			e.printStackTrace();
		}
	}
	
	/**
	 * Gets the dependents of class c. Does not write to the file.  If c does not appear to be in the manifest, refreshes. Returns
	 * null if c cannot be found.
	 * 
	 * @param c the plugin class
	 * @return the classes that a given plugin uses
	 */
	public ArrayList<Class> getEntry(Class c) {
		if(manifestInfo.get(c) == null) {
			refresh();
		}
		
		ArrayList<Class> entryInfo = manifestInfo.get(c);
		return entryInfo;
	}
	
	/**
	 * Removes class c and its dependents from the manifest, does in fact write to the file. If c does not appear to be a plugin 
	 * in the manifest, refreshes and returns null if not found.  Returns all dependents that are not currently used by
	 * another entry in the manifest (either as a plugin or one of a plugin's dependents).
	 * 
	 * @param c
	 * @return 
	 */
	public ArrayList<Class> removeEntry(Class c) {
		if(manifestInfo.get(c) == null) {
			refresh();
		}
		
		ArrayList<Class> dep = manifestInfo.remove(c);
		if(dep != null) {
			removeFromFile(c);
			
			boolean depExists;
			Class currentClass;
			int i = 0;
	depQ:	while(dep.size() > i) {
				depExists = false;
				currentClass = dep.get(i);
				Iterator<Class> itr = manifestInfo.keySet().iterator();
				while(itr.hasNext()) {
					Class plugin = itr.next();
					if(plugin.equals(currentClass)) {
						depExists = true;
						break depQ;
					}
					ArrayList<Class> depList = manifestInfo.get(plugin);
					for(int j=0; j<depList.size(); j++) {
						if(depList.get(j).equals(currentClass)) {
							depExists = true;
							break depQ;
						}
					}
				}
				if(depExists) {
					dep.remove(currentClass);
				} else {
					i++;
				}
			}
		}
		return dep;
	}
	
	/**
	 * Helper method for removing c from the manifest file on the file system.
	 * @param c the class to be removed.
	 */
	private void removeFromFile(Class c) {
		BufferedReader in = null;
		BufferedWriter out = null;
		File temp = null;
		try {
			in = new BufferedReader(new FileReader(manifest));
			out = new BufferedWriter(new FileWriter(temp = new File(manifest.getAbsolutePath()+manifest.getName()+".tmp")));
		
			if(in != null && out != null) {
				String nextLine = new String();
				boolean inPlugin = false;
				while((nextLine = in.readLine()) != null) {
					if(nextLine.equals(PLUGIN_ENTRY+DELIMITER+c.getName())) { //line-delim has been stripped
						inPlugin = true;
					} else if(inPlugin) {
						if(nextLine.contains(PLUGIN_ENTRY)) {
							inPlugin = false;
							out.write(nextLine+LINE_DELIM); //line-delim has been stripped
						}
					} else {
						out.write(nextLine+LINE_DELIM); //line-delim has been stripped
					}
				}
				
				in.close();
				
				out.flush();
				out.close();
			}
			
			writeTmpToManifest(temp);
		} catch (FileNotFoundException e) {
			MipavUtil.displayInfo("Unable to locate manifest file.");
			e.printStackTrace();
		} catch (IOException e) {
			MipavUtil.displayInfo("Unable to write to manifest file.");
			e.printStackTrace();
		}
	}
	
	private void writeTmpToManifest(File temp) {
		BufferedReader in = null;
		BufferedWriter out = null;
		try {
			in = new BufferedReader(new FileReader(temp));
			out = new BufferedWriter(new FileWriter(manifest));
			
			if(in != null && out != null) {
				String nextLine = new String();
				while((nextLine = in.readLine()) != null) {
					out.write(nextLine+LINE_DELIM);
				}
				
				in.close();
				
				out.flush();
				out.close();
				
				temp.delete();
			}
		} catch (FileNotFoundException e) {
			MipavUtil.displayInfo("Unable to locate manifest file.");
			e.printStackTrace();
		} catch (IOException e) {
			MipavUtil.displayInfo("Unable to write to manifest file.");
			e.printStackTrace();
		}
	}

	/**
	 * Inserts all the dependents that are not currently a dependent into c's entry into the manifest.  If entry c doesn't appear
	 * to exist in the manifest, reloads the manifest and performs an addEntry if necessary.
	 * 
	 * @see addEntry(Class, Class...)
	 * 
	 * @param c
	 * @param dependents
	 * @return The full entry for c
	 */
	public ArrayList<Class> modifyEntry(Class c, Class... dependents) {
		ArrayList<Class> pluginInfo = getEntry(c);
		BufferedReader in = null;
		BufferedWriter out = null;
		File temp = null;
		
		try {
			in = new BufferedReader(new FileReader(manifest));
			out = new BufferedWriter(new FileWriter(temp = new File(manifest.getAbsolutePath()+manifest.getName()+".tmp")));
		
		
			if(in != null && out != null) {
				String nextLine = new String();
				boolean inPlugin = false;
				while((nextLine = in.readLine()) != null) {
					if(nextLine.equals(PLUGIN_ENTRY+DELIMITER+c.getName())) {
		
					inPlugin = true;
					} else if(inPlugin) {
						for(int i=0; i<dependents.length; i++) {
							if(!pluginInfo.contains(dependents[i])) {
								pluginInfo.add(dependents[i]);
								out.write(DEPENDENT_ENTRY+DELIMITER+dependents[i].toString()+LINE_DELIM);
							}
						}
						if(nextLine.contains(PLUGIN_ENTRY)) {
							inPlugin = false;
							out.write(nextLine+LINE_DELIM);
						}
					} else {
						out.write(nextLine+LINE_DELIM);
					}
				}
				
				in.close();
				
				out.flush();
				out.close();
				
				writeTmpToManifest(temp);
			}
		} catch (FileNotFoundException e) {
			MipavUtil.displayInfo("Unable to locate manifest file.");
			e.printStackTrace();
		} catch (IOException e) {
			MipavUtil.displayInfo("Unable to write to manifest file.");
			e.printStackTrace();
		}
		
		return pluginInfo;
	}

	private void createManifestFile() {
		manifest.getParentFile().mkdirs();
		try {
			manifest.createNewFile();
			BufferedWriter out = new BufferedWriter(new FileWriter(manifest));
			out.write("/****MIPAV plugin manifest file****/"+LINE_DELIM);
			out.write("/****Plugin dependencies are written below this line****/"+LINE_DELIM+LINE_DELIM);
			
			out.flush();
			out.close();		
		} catch (IOException e) {
			MipavUtil.displayInfo("Manifest file unable to be created, ensure you have access to your home directory");
		}
	}
}
