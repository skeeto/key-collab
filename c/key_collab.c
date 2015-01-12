#define _XOPEN_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include "platform.h"

#define NUM_WORDS 7260
#define WORD_LEN  6
#define KEY_LEN   26
#define MAX_TRIES 4096

static char wordlist[NUM_WORDS][WORD_LEN + 1];

/* NOTE: Assuming an int can be read and written atomically. */
static struct {
    int score;
    char key[KEY_LEN + 1];
} best;

/* NOTE: Assuming an unsigned long int can be read atomically. */
struct worker_data {
    int id;
    unsigned long count;
    unsigned long last_count;
    platform_mutex_t display_lock;
};

extern const char _binary_wordlist_start;

static void wordlist_load(void)
{
    const char *p = &_binary_wordlist_start;
    for (int i = 0; i < NUM_WORDS; i++) {
        for (int c = 0; c < WORD_LEN; c++)
            wordlist[i][c] = *p++;
        p += 2;
    }
}

static int key_score(char *key)
{
    int score = 0;
    for (int i = 0; i < NUM_WORDS; i++) {
        bool sorted = true;
        for (int c = 1; c < WORD_LEN; c++) {
            if (key[wordlist[i][c] - 'a'] < key[wordlist[i][c - 1] - 'a']) {
                sorted = false;
                break;
            }
        }
        if (sorted)
            score++;
    }
    return score;
}

static inline void key_swap(char *key, int i, int j)
{
    char tmp = key[j];
    key[j] = key[i];
    key[i] = tmp;
}

static inline void key_shuffle(char *key)
{
    for (int i = KEY_LEN - 1; i > 0; i--)
        key_swap(key, i, rand() % (i + 1));
}

static inline void key_perturb(char *key, int n)
{
    for (int i = 0; i < n; i++)
        key_swap(key, rand() % KEY_LEN, rand() % KEY_LEN);
}

static inline void key_copy(char *dest, const char *src)
{
    memcpy(dest, src, KEY_LEN);
}

static void print(int row, const char *format, ...)
{
    char buffer[80];
    va_list ap;
    va_start(ap, format);
    vsprintf(buffer, format, ap);
    va_end(ap);
    platform_terminal_move(row);
    puts(buffer);
}

static void worker(struct worker_data *data)
{
    char current_key[] = "abcdefghijklmnopqrstuvwxyz";
    key_shuffle(current_key);
    int current_score = key_score(current_key);

    for (int key_count = 0; ; data->count++, key_count++) {
        char temp[KEY_LEN + 1];
        if (key_count > MAX_TRIES) {
            key_shuffle(current_key);
            current_score = key_score(current_key);
            key_count = 0;
            key_copy(temp, current_key);
        } else {
            key_copy(temp, current_key);
            key_perturb(temp, (key_count * KEY_LEN) / MAX_TRIES + 1);
        }

        int score = key_score(temp);
        if (score > best.score) {
            platform_mutex_lock(data->display_lock);
            if (score > best.score) { // check again under lock
                best.score = score;
                key_copy(best.key, current_key);
                print(0, "best:     %s (%d)", best.key, best.score);
            }
            platform_mutex_unlock(data->display_lock);
        }
        if (score > current_score) {
            key_copy(current_key, temp);
            current_score = score;
            key_count = 0;
            platform_mutex_lock(data->display_lock);
            print(data->id + 2, "thread %d: %s (%d)",
                  data->id, current_key, current_score);
            platform_mutex_unlock(data->display_lock);
        }
    }
}

int main(void)
{
    srand(time(NULL));
    wordlist_load();
    platform_terminal_init();
    platform_mutex_t display_lock = platform_mutex_create();
    int ncores = platform_numcores();

    struct worker_data worker_data[ncores];
    for (int i = 0; i < ncores; i++) {
        worker_data[i].id = i;
        worker_data[i].count = 0;
        worker_data[i].last_count = 0;
        worker_data[i].display_lock = display_lock;
        platform_thread(&worker, &worker_data[i]);
    }

    for (;;) {
        platform_sleep(1);
        int total = 0;
        for (int i = 0; i < ncores; i++) {
            unsigned long count = worker_data[i].count;
            total += count - worker_data[i].last_count;
            worker_data[i].last_count = count;
        }
        platform_mutex_lock(display_lock);
        print(1, "rate:     %d keys/sec\n", total);
        platform_mutex_unlock(display_lock);
    }

    platform_terminal_free();
    return 0;
}
