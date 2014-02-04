/* port_driver.c */
// gcc -o exampledrv -fpic -shared complex.c port_driver.c

#include <stdio.h>
#include "/usr/local/lib/erlang/usr/include/erl_driver.h"

typedef struct {
    ErlDrvPort port;
} example_data;

static ErlDrvData example_drv_start(ErlDrvPort port, char *buff)
{
    example_data* d = (example_data*)driver_alloc(sizeof(example_data));
    d->port = port;
    return (ErlDrvData)d;
}

static void example_drv_stop(ErlDrvData handle)
{
    driver_free((char*)handle);
}


int plus_one(int x) {
  return x+1;
}

int times_two(int y) {
  return y*2;
}


static void example_drv_output(ErlDrvData handle, char *buff, int bufflen)
{
    example_data* d = (example_data*)handle;
    char fn = buff[0], arg = buff[1], res;
    if (fn == 1) {
      res = plus_one(arg);
    } else if (fn == 2) {
      res = times_two(arg);
    }
    driver_output(d->port, &res, 1);
}


ErlDrvEntry example_driver_entry = {
    NULL,               /* F_PTR init, N/A */
    example_drv_start,  /* L_PTR start, called when port is opened */
    example_drv_stop,   /* F_PTR stop, called when port is closed */
    example_drv_output, /* F_PTR output, called when erlang has sent
			   data to the port */
    NULL,               /* F_PTR ready_input,
                           called when input descriptor ready to read*/
    NULL,               /* F_PTR ready_output,
                           called when output descriptor ready to write */
    "example_drv",     /* char *driver_name, the argument to open_port */
    NULL,               /* F_PTR finish, called when unloaded */
    NULL,               /* F_PTR control, port_command callback */
    NULL,               /* F_PTR timeout, reserved */
    NULL                /* F_PTR outputv, reserved */
};

DRIVER_INIT(example_drv) /* must match name in driver_entry */
{
    return &example_driver_entry;
}


