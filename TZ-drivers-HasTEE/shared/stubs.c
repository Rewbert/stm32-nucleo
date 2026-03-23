#include <sys/stat.h>
#include <sys/times.h>
#include <sys/types.h>
#include <errno.h>

#include "drivers/uart.h"
#include "boards/board.h"

/* Newlib nano syscall stubs. Most are meaningless in a bare-metal context. */

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
    (void) file;
    uart_read(board_console(), (uint8_t *)ptr, len);
    return len;
}

register char * stack_ptr asm("sp");

caddr_t _sbrk(int incr) {
    extern char _end;
    static char *heap_end;
    char *prev_heap_end;

    if (heap_end == 0) {
        heap_end = &_end;
    }
    prev_heap_end = heap_end;
    if (heap_end + incr > stack_ptr) {
        while (1) {}
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

int _write(int file, char *ptr, int len) {
    (void) file;
    uart_write(board_console(), (uint8_t *)ptr, len);
    return len;
}

void _exit(int n) {
    while (1) {}
}
