/* c-repl -- a C read-eval-print loop.
 * Copyright (C) 2006 Evan Martin <martine@danga.com>
 */

/* The child process is what actually runs the code.
 * It reads in a number from stdin,
 * then loads dl#.so and executes dl#().
 */

#include <assert.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static int debug = 0;

/* Load dl<id>.so and run dl<id>(). */
static void load_and_run(int id) {
  char buf[1024];

  sprintf(buf, "./.c-repl/dl%d.so", id);
  if (debug)
    fprintf(stderr, "CHILD> loading %s\n", buf);
  void *so = dlopen(buf, RTLD_LAZY | RTLD_GLOBAL);
  if (!so) {
    fprintf(stderr, "CHILD> error loading library: %s\n", dlerror());
    assert(so);
  }

  sprintf(buf, "dl%d", id);
  void (*f)() = dlsym(so, buf);
  if (!f) {
    fprintf(stderr, "CHILD> error loading function: %s\n", dlerror());
    assert(f);
  }

  //printf("child executing '%s':\n", buf);
  // XXX fork here to do the segfault -> undo magic?
  f();
}

int main(int argc, char **argv) {
  if (argc < 3) {
    fprintf(stderr, "bad arguments\n");
    return 1;
  }
  const int command_fd = atoi(argv[1]);
  const int response_fd = atoi(argv[2]);

  FILE* command_pipe = fdopen(command_fd, "rb");
  if (!command_pipe) {
    perror("fdopen(command_fd)");
    return 1;
  }
  setlinebuf(command_pipe);
  FILE* response_pipe = fdopen(response_fd, "wb");
  if (!response_pipe) {
    perror("fdopen(response_fd)");
    return 1;
  }
  setlinebuf(response_pipe);

  if (fprintf(response_pipe, "%d\n", getpid()) < 0) {
    perror("CHILD> fputs");
    return 1;
  }

  char buf[1024];
  int highest_id = 0;
  while (fgets(buf, sizeof(buf), command_pipe)) {
    const int id = atoi(buf);
    if (id > 0)
      for ( ; highest_id < id; highest_id++)
        load_and_run(highest_id+1);

    /* If we get here, we succeeded.
     * Let the parent know. */
    if (fputs(buf, response_pipe) < 0) {
      perror("CHILD> fputs");
      break;
    }
    fflush(response_pipe);
  }
  if (debug)
    fprintf(stderr, "CHILD> exiting\n");

  fclose(command_pipe);
  fclose(response_pipe);

  return 0;
};

/* vim: set ts=2 sw=2 et cino=(0 : */
