package gov.nih.mipav.model.algorithms.t2mapping.cj.utilities;

import java.util.logging.*;
import java.util.*;
import java.text.*;

/** The beginnings of code to help in using the logger. */
public class CJLogger
{
	/** Create a logger and set the default level to be INFO. */
	static public Logger createLogger(String loggername)
	{
		Logger logger = Logger.getLogger(loggername);
		logger.setLevel(Level.INFO);
		for(int ii=0; ii<logger.getHandlers().length;ii++)
		{
			((logger.getHandlers())[ii]).setLevel(Level.INFO);
			((logger.getHandlers())[ii]).setFormatter( new CraigFormat() );
		}

		return logger;
	}

	/** Set the level of the logger and all associated handlers. */
	static public void setLevel(Logger logger, Level level)
	{
		logger.setLevel(level);
		Handler[] handlers = logger.getHandlers();
		for(int ii=0; ii<handlers.length;ii++)
			handlers[ii].setLevel(level);
	}

}

/**
 *  My own formatter that extends the SimpleFormatter which is prety
 *  ugly.
 */
class CraigFormat extends SimpleFormatter
{
	public String format(LogRecord record) 
	{
		SimpleDateFormat sdf = new SimpleDateFormat("HH:mm:ss.S");

		String output = "";
		output += record.getSourceClassName()+":"+record.getSourceMethodName();
		output += " ["; 
		output += sdf.format(new Date(record.getMillis()));
		output += "]: ";
		output += record.getMessage() + "\n";

 
// "h:mm a"

		return output;
	}
}
