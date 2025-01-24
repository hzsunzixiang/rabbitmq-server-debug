#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
// yum install systemd-devel
#include <systemd/sd-daemon.h>

int main(int argc, char *argv[]) {
	printf("Starting up ...");
	sleep(5);
	printf("Startup complete before notify");
	sd_notify(0, "READY=1");
	sleep(5);
	sd_notify(0, "STATUS=status message");
	sleep(5);

	for(;;) 
	{
		printf("Hello from the Demo Service");
		sd_notify(0, "STATUS=Processing  data");
		sleep(5);
		sd_notify(0, "STATUS=Waiting for data");
		sleep(5);
	}
	return 0;
}


// sudo yum install systemd-devel
// gcc -o my-program helloworld.c -lsystemd
//
