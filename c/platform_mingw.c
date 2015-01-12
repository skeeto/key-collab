#include <windows.h>
#include <process.h>
#include "platform.h"

struct platform_worker {
    worker_t worker;
    struct worker_data *data;
};

static void platform_worker(void *p)
{
    struct platform_worker *worker = p;
    worker->worker(worker->data);
    free(p);
}

void platform_thread(worker_t worker, struct worker_data *data)
{
    struct platform_worker *p = malloc(sizeof(*p));
    *p = (struct platform_worker){worker, data};
    _beginthread(platform_worker, 0, p);
}

struct platform_mutex {
    CRITICAL_SECTION mutex;
};

platform_mutex_t platform_mutex_create(void)
{
    platform_mutex_t mutex = malloc(sizeof(*mutex));
    InitializeCriticalSectionAndSpinCount(&mutex->mutex, 0x0400);
    return mutex;
}

void platform_mutex_free(platform_mutex_t mutex)
{
    DeleteCriticalSection(&mutex->mutex);
    free(mutex);
}

void platform_mutex_lock(platform_mutex_t mutex)
{
    EnterCriticalSection(&mutex->mutex);
}

void platform_mutex_unlock(platform_mutex_t mutex)
{
    LeaveCriticalSection(&mutex->mutex);
}

void platform_terminal_init(void)
{
    COORD coord = {0, 0};
    HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    DWORD out;
    FillConsoleOutputCharacter(handle, ' ', -1, coord, &out);
}

void platform_terminal_move(int row)
{
    COORD coord = {0, row};
    HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    SetConsoleCursorPosition(handle, coord);
}

void platform_terminal_free(void)
{
    /* Nothing */
}

int platform_numcores(void)
{
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return sysinfo.dwNumberOfProcessors;
}

void platform_sleep(int seconds)
{
    Sleep(seconds * 1000);
}
