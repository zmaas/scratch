#include <mpi.h>
#include <cstdlib>
#include <ctime>
#include <iomanip>
#include <iostream>

using namespace std;

int main(int argc, char *argv[]);
void timestamp();

int main(int argc, char *argv[]) {
        int id;
        int ierr;
        int p;
        double wtime;
        //
        //  Initialize MPI.
        //
        ierr = MPI_Init(&argc, &argv);
        //
        //  Get the number of processes.
        //
        ierr = MPI_Comm_size(MPI_COMM_WORLD, &p);
        //
        //  Get the individual process ID.
        //
        ierr = MPI_Comm_rank(MPI_COMM_WORLD, &id);
        //
        //  Process 0 prints an introductory message.
        //
        if (id == 0) {
                timestamp();
                cout << "\n";
                cout << "HELLO_MPI - Master process:\n";
                cout << "  C++/MPI version\n";
                cout << "  An MPI example program.\n";
                cout << "\n";
                cout << "  The number of processes is " << p << "\n";
                cout << "\n";
        }
        //
        //  Every process prints a hello.
        //
        if (id == 0) {
                wtime = MPI_Wtime();
        }
        cout << "  Process " << id << " says 'Hello, world!'\n";
        //
        //  Process 0 says goodbye.
        //
        if (id == 0) {
                wtime = MPI_Wtime() - wtime;
                cout << "  Elapsed wall clock time = " << wtime
                     << " seconds.\n";
        }
        //
        //  Terminate MPI.
        //
        MPI_Finalize();
        //
        //  Terminate.
        //
        if (id == 0) {
                cout << "\n";
                cout << "HELLO_MPI:\n";
                cout << "  Normal end of execution.\n";
                cout << "\n";
                timestamp();
        }
        return 0;
}

void timestamp() {
#define TIME_SIZE 40

        static char time_buffer[TIME_SIZE];
        const struct std::tm *tm_ptr;
        size_t len;
        std::time_t now;

        now = std::time(NULL);
        tm_ptr = std::localtime(&now);

        len = std::strftime(time_buffer, TIME_SIZE, "%d %B %Y %I:%M:%S %p",
                            tm_ptr);

        std::cout << time_buffer << "\n";

        return;

#undef TIME_SIZE
}
