
if __name__ == '__main__':
    import time
    import systemd.daemon

    print('Starting up ...')
    time.sleep(5)
    print('Startup complete before notify')
    systemd.daemon.notify('READY=1')
    print('Startup complete after notify')

    while True:
        print('Hello from the Python Demo Service')
        systemd.daemon.notify("STATUS=Processing  data")
        time.sleep(5)
        systemd.daemon.notify("STATUS=Waiting for data")
        time.sleep(5)

