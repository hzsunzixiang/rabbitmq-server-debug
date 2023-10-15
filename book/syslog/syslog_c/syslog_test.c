#include <syslog.h>
int main() {
	openlog("Logs", LOG_PID, LOG_USER);
	syslog(LOG_INFO, "Start logging");
	closelog();
}
