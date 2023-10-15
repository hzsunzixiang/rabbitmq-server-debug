
import syslog

syslog.syslog('Processing started')
#if error:
#    syslog.syslog(syslog.LOG_ERR, 'Processing started')

#An example of setting some log options, these would include the process ID in logged messages, and write the messages to the destination facility used for mail logging:

syslog.openlog(logoption=syslog.LOG_PID, facility=syslog.LOG_USER)
syslog.syslog('E-mail processing initiated...')
