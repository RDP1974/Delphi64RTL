// 20 jun 2026 Roberto Della Pasqua - www.dellapasqua.com
#include "pch.h"
#include <tbb/concurrent_queue.h>
#include <windows.h>

extern "C" BOOL WINAPI DllMain(HINSTANCE, DWORD callReason, LPVOID lpvReserved)
{
    return TRUE;
}

extern "C" {

    // Crea una nuova coda (ritorna un puntatore alla queue)
    __declspec(dllexport) tbb::concurrent_queue<void*>* __stdcall CreateQueue() {
        return new tbb::concurrent_queue<void*>();
    }

    // Libera la memoria della queue
    __declspec(dllexport) void __stdcall FreeQueue(tbb::concurrent_queue<void*>* queue) {
        delete queue;
    }

    // Aggiunge un puntatore alla coda
    __declspec(dllexport) void __stdcall PushToQueue(tbb::concurrent_queue<void*>* queue, void* ptr) {
        if (queue) queue->push(ptr);
    }

    // Estrae un puntatore dalla coda
    __declspec(dllexport) bool __stdcall PopFromQueue(tbb::concurrent_queue<void*>* queue, void** outPtr) {
        if (queue && outPtr && queue->try_pop(*outPtr))
            return true;
        return false;
    }

    // Verifica se la coda è vuota
    __declspec(dllexport) bool __stdcall IsQueueEmpty(tbb::concurrent_queue<void*>* queue) {
        return queue == nullptr || queue->empty();
    }

}