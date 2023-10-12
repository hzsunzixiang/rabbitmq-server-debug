#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <systemd/sd-daemon.h>

int main(int argc, char *argv[]) {
    printf("Starting up ...");
    sleep(10);
    printf("Startup complete before notify");

    // Send a READY message to systemd
    sd_notify(0, "READY=1");

    // Send a STATUS message to systemd
    sd_notify(0, "STATUS=status message");

    // Send a WATCHDOG message to systemd
    //sd_notify(0, "WATCHDOG=1");

    for(;;) 
	{
        printf("Hello from the Python Demo Service");
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
