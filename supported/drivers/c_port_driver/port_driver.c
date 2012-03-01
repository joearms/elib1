#include <stdio.h>
#include <unistd.h>

typedef unsigned char byte;

int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);

byte out[128];

int read_cmd(byte *buf)
{
  int len;

  if (read_exact(buf, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_cmd(byte *buf, int len)
{
  byte li;

  li = (len >> 8) & 0xff;
  write_exact(&li, 1);
  
  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}

int read_exact(byte *buf, int len)
{
  int i, got=0;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);

  return(len);
}

int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}

int main() {
  int fn, arg1, arg2, result, n;
  byte buff[100];

  while (n = read_cmd(buff), n > 0) {
    handle(n, buff);
  }
}

// This is were we do all the work

handle(int n, byte* in){
  if (in[0] == 1) {
    // double
    out[0] = 2 * in[1];
    write_cmd(out, 1);
  } else if (in[0] == 2) {
    // sum
    out[0] = in[1] + in[2];
    write_cmd(out, 1);
  };
}
