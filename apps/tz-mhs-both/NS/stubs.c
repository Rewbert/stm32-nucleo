#include <sys/stat.h>
#include <sys/times.h>
#include <sys/types.h>
#include <errno.h>

#include "hal/uart.h"

/* These are the syscalls. Newlib nano wants them to be prefixed with an underscore.
Many of them are completely meaningless (so far). */

int _close(int file) {
    return -1;
}

char *__env[1] = { 0 };
char **environ = __env;

int execve(char *name, char **argv, char **env) {
    errno = ENOMEM;
    return -1;
}

int fork(void) {
    errno = EAGAIN;
    return -1;
}

int _fstat(int file, struct stat *st) {
    st->st_mode = S_IFCHR;
    return 0;
}

int _getpid(void) {
    return 1;
}

int _isatty(int file) {
    return 1;
}

int _kill(int pid, int sig) {
    errno = EINVAL;
    return -1;
}

int link(char *old, char *new) {
    errno = EMLINK;
    return -1;
}

int _lseek(int file, int ptr, int dir) {
    return 0;
}

int _open(const char *name, int flags, int mode) {
    return -1;
}

int _read(int file, char *ptr, int len) {
    (void) file; // always read LPUART, don't actually use the file handles

    int i;
    for(i = 0; i < len; i++) {
    ptr[i] = platform_lpuart1_read();
    }

    return len;
}

register char * stack_ptr asm("sp");

/* Grab a chunk of memory from the heap, called by malloc to populate its free list */
caddr_t _sbrk(int incr) {
    //extern char __bss_end__;		/* Defined by the linker */
    extern char _end;
    static char *heap_end;
    char *prev_heap_end;

    if (heap_end == 0) { // first time we call this, heap_end is 0
    heap_end = &_end;
    }
    prev_heap_end = heap_end;
    if (heap_end + incr > stack_ptr) {
    while (1) {

    }
    }

    heap_end += incr;
    return (caddr_t) prev_heap_end;
}

int stat(const char *file, struct stat *st) {
    st->st_mode = S_IFCHR;
    return 0;
}


int unlink(char *name) {
    errno = ENOENT;
    return -1; 
}


/* Always write to LPUART, don't care about other file handles */
int _write(int file, char *ptr, int len) {
    (void) file;
    int todo;

    for (todo = 0; todo < len; todo++) {
    platform_lpuart1_write(*ptr++);
    }
    return len;
}

int _exit(int) {
    return 0;
}