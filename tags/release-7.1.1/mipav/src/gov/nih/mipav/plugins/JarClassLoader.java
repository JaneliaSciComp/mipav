package gov.nih.mipav.plugins;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLStreamHandlerFactory;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.TreeSet;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;

public class JarClassLoader extends URLClassLoader {

	private URLClassLoader internalClassLoader = null;
	
	private HashMap<String, Class> cachedClass;
	
	public JarClassLoader(ClassLoader c) throws MalformedURLException {
		this(new URL[]{new URL("jar:file:/Users/justinsenseney/mipav/plugins/testjar.jar!/")}, c);
		
	}
	
	public JarClassLoader(URL[] arg0, ClassLoader arg1, URLStreamHandlerFactory arg2) {
		super(arg0, arg1, arg2);
		init(arg0, arg1, arg2);
		System.out.println("Successfully created jar class loader1");
	}

	public JarClassLoader(URL[] arg0, ClassLoader arg1) {
		super(arg0, arg1);
		init(arg0, arg1, null);
		System.out.println("Successfully created jar class loader2");
		
	}
	
	private void init(URL[] arg0, ClassLoader arg1, URLStreamHandlerFactory arg2) {
		if(arg2 != null) {
			internalClassLoader = new URLClassLoader(arg0, arg1, arg2);
		} else {
			internalClassLoader = new URLClassLoader(arg0, arg1);
		}
		cachedClass = new HashMap<String, Class>();
	}

	public boolean addJarContext(String context) throws MalformedURLException {
		if(isContext(context)) {
			return true;
		}
		URL url = contextStringToURL(context);
		if(url == null) {
			return false;
		}
		URL[] urlList = new URL[internalClassLoader.getURLs().length+1];
		for(int i=0; i<urlList.length-1; i++) {
			urlList[i] = internalClassLoader.getURLs()[i];
		}
		urlList[internalClassLoader.getURLs().length] = url;
		internalClassLoader = URLClassLoader.newInstance(urlList);
		return true;
	}
	
	public boolean removeJarContext(String context) {
		URL url = contextStringToURL(context);
		if(url == null || internalClassLoader == null) {
			return false;
		}
		
		URL[] urlList = internalClassLoader.getURLs();
		int indexRemove = -1;
		for(int i=0; i<urlList.length; i++) {
			if(urlList[i].equals(url)) {
				indexRemove = i;
				break;
			}
		}
		
		if(indexRemove == -1) {
			return false;
		}
		
		URL[] urlNewList = new URL[urlList.length-1];
		for(int i=0, j=0; i<urlNewList.length; i++, j++) {
			if(i == indexRemove) {
				j++;
			}
			urlNewList[i] = urlList[j];
		}
		
		internalClassLoader = URLClassLoader.newInstance(urlNewList);
		return true;
	}
	
	private URL contextStringToURL(String context) {
		try {
			URL url = new URL("jar:file:"+context+"!/");
			return url;
		} catch(MalformedURLException ex) {
			ex.printStackTrace();
			MipavUtil.displayError("Bad jar name specified.");
		}
		
		return null;
	}
	
	public void clearAllContexts() {
		internalClassLoader = null;
	}
	
	public boolean isContext(String context) throws MalformedURLException {
		URL url =  contextStringToURL(context);
		URL[] urlList = internalClassLoader.getURLs();
		for(int i=0; i<urlList.length; i++) {
			if(url.equals(urlList[i])) {
				return true;
			}
		}
		return false;
	}
	
	
	@Override
	public URL getResource(String name) {
		MipavUtil.displayInfo("Successfully calling jar class loader");
		URL url = super.getResource(name);
		if(url == null && internalClassLoader != null) {
			if(name.charAt(0) == '/') {
				name = name.substring(1);
			} 
				
			return internalClassLoader.getResource(name);
		} else {
			return url;
		}
	}

	@Override
	public InputStream getResourceAsStream(String name) {
		MipavUtil.displayInfo("Successfully calling jar class loader");
		InputStream is = super.getResourceAsStream(name);
		if(is == null && internalClassLoader != null) {
			if(name.charAt(0) == '/') {
				name = name.substring(1);
			} 
				
			return internalClassLoader.getResourceAsStream(name);
		} else {
			return is;
		}
	}

	@Override
	public Enumeration<URL> getResources(String name) throws IOException {
		MipavUtil.displayInfo("Successfully calling jar class loader");
		Enumeration<URL> e = super.getResources(name);
		if(e == null && internalClassLoader != null) {
			if(name.charAt(0) == '/') {
				name = name.substring(1);
			} 
				
			return internalClassLoader.getResources(name);
		} else {
			return e;
		}
	}

	@Override
	protected synchronized Class<?> loadClass(String className, boolean resolveIt)
			throws ClassNotFoundException {
		
		System.out.println("Loading class: "+className);
		Class result;
		byte[] classData;
		result = (Class)cachedClass.get(className);
		if(result != null) {
			return result;
		}
		if(internalClassLoader != null) {
			result = internalClassLoader.loadClass(className);
			
			if(result != null) {
				return result;
			}
		}
		
		try {
			result = super.findSystemClass(className);
			return result;
		} catch(ClassNotFoundException ex) {
			System.out.println("Unable to find system class: "+className);
			return null;
		}
	}

	@Override
	public Class<?> loadClass(String name) throws ClassNotFoundException {
		MipavUtil.displayInfo("Successfully calling jar class loader");
		Class c = super.loadClass(name);
		if(c == null && internalClassLoader != null) {
			if(name.charAt(0) == '/') {
				name = name.substring(1);
			} 
				
			return internalClassLoader.loadClass(name);
		} else {
			return c;
		}
	}

	@Override
	protected Class<?> findClass(String name) throws ClassNotFoundException {
		Class<?> c = super.findClass(name);
		if(c == null) {
			MipavUtil.displayError("Warning, findClass not uniquely implemented");//not designed to be necessary
		}
		return c;
	}

	@Override
	public URL findResource(String name) {
		MipavUtil.displayInfo("Successfully calling jar class loader");
		URL url = super.findResource(name);
		if(url == null && internalClassLoader != null) {
			if(name.charAt(0) == '/') {
				name = name.substring(1);
			} 
				
			return internalClassLoader.findResource(name);
		} else {
			return url;
		}
	}

	@Override
	public Enumeration<URL> findResources(String name) throws IOException {
		MipavUtil.displayInfo("Successfully calling jar class loader");
		Enumeration<URL> e = super.findResources(name);
		if(e == null && internalClassLoader != null) {
			if(name.charAt(0) == '/') {
				name = name.substring(1);
			} 
				
			return internalClassLoader.findResources(name);
		} else {
			return e;
		}
	}

	@Override
	public URL[] getURLs() {
		MipavUtil.displayInfo("Successfully calling jar class loader");
		URL[] beginList = super.getURLs();
		if(internalClassLoader != null) {
			URL[] endList = internalClassLoader.getURLs();
			URL[] totalList = new URL[beginList.length+endList.length];
			for(int i=0; i<beginList.length; i++) {
				totalList[i] = beginList[i];
			}
			for(int i=beginList.length; i<beginList.length+endList.length; i++) {
				totalList[i] = endList[i-beginList.length];
			}
			return totalList;
		} else {
			return beginList;
		}
	}

	public JarClassLoader(URL[] urls) {
		super(urls);
		internalClassLoader = new URLClassLoader(urls);
	}
	
	public static URLClassLoader newInstance(URL[] arg0) {
		return new JarClassLoader(arg0);
	}

}
