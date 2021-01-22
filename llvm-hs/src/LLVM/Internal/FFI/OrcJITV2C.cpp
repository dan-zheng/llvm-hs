#include <iostream>

#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/Module.h>
#include <llvm-c/Core.h>

#include "LLVM/Internal/FFI/Target.hpp"

using namespace llvm;
using namespace orc;

extern "C" {

// Thread-safe context

ThreadSafeContext* LLVM_Hs_createThreadSafeContext() {
    return new ThreadSafeContext(std::make_unique<LLVMContext>());
}

void LLVM_Hs_disposeThreadSafeContext(ThreadSafeContext* ctx) {
    delete ctx;
}

// Thread-safe module

ThreadSafeModule* LLVM_Hs_createThreadSafeModule(LLVMModuleRef m) {
    auto moduleClone = LLVMCloneModule(m);
    std::unique_ptr<Module> module{unwrap(moduleClone)};
    llvm::errs() << "LLVM_Hs_createThreadSafeModule: " << module.get() << "\n";
    return new ThreadSafeModule(std::move(module), std::make_unique<LLVMContext>());
}

void LLVM_Hs_disposeThreadSafeModule(ThreadSafeModule* module) {
    llvm::errs() << "LLVM_Hs_disposeThreadSafeModule: " << module->getModuleUnlocked() << "\n";
    if (module == nullptr) {
        return;
    }
    delete module;
}

// Object layer

ObjectLayer* LLVM_Hs_createRTDyldObjectLinkingLayer(ExecutionSession* es) {
    return new RTDyldObjectLinkingLayer(*es, []() {
        return std::make_unique<SectionMemoryManager>();
    });
}

void LLVM_Hs_disposeObjectLayer(ObjectLayer* ol) {
    // delete ol;
}

// Compile layer

IRLayer* LLVM_Hs_createIRCompileLayer(ExecutionSession* es, ObjectLayer* baseLayer, LLVMTargetMachineRef tm) {
    return new IRCompileLayer(*es, *baseLayer, std::make_unique<SimpleCompiler>(SimpleCompiler(*unwrap(tm))));
}

void LLVM_Hs_disposeIRLayer(IRLayer* il) {
    delete il;
}

// Warning: This consumes the module.
void LLVM_Hs_IRLayer_add(ThreadSafeModule* tsm, JITDylib* dylib, LLVMTargetDataRef dataLayout, IRLayer* il) {
    tsm->withModuleDo([&](auto& module) {
        if (module.getDataLayout().isDefault()) {
            module.setDataLayout(*unwrap(dataLayout));
        }
    });
    // NOTE: Maybe try module cloning?
    llvm::errs() << "LLVM_Hs_IRLayer_add: " << tsm->getModuleUnlocked() << "\n";
    if (Error err = il->add(*dylib, std::move(*tsm))) {
        llvm::errs() << err << "\n";
        exit(1);
    }
    llvm::errs() << "LLVM_Hs_IRLayer_add after: " << tsm->getModuleUnlocked() << "\n";
}

JITDylib* LLVM_Hs_ExecutionSession_createJITDylib(ExecutionSession* es, const char* name) {
    if (auto dylibOrErr = es->createJITDylib(name)) {
        auto& dylib = *dylibOrErr;
        return &dylib;
    } else {
        Error err = dylibOrErr.takeError();
        llvm::errs() << err << "\n";
        exit(1);
    }
}

JITDylib* LLVM_Hs_ExecutionSession_getJITDylibByName(ExecutionSession* es, const char* name) {
    return es->getJITDylibByName(name);
}

uint64_t LLVM_Hs_ExecutionSession_lookup(ExecutionSession* es, JITDylib *dylib, const char* mangledName) {
    llvm::errs() << "LLVM_Hs_ExecutionSession_lookup start\n";
    es->dump(llvm::errs());
    llvm::errs() << "LLVM_Hs_ExecutionSession_lookup next\n";
    if (auto symbolOrErr = es->lookup({dylib}, mangledName)) {
        auto& symbol = *symbolOrErr;
        llvm::errs() << "LLVM_Hs_ExecutionSession_lookup end\n";
        return symbol.getAddress();
    } else {
        Error err = symbolOrErr.takeError();
        llvm::errs() << err << "\n";
        exit(1);
    }
}

}
