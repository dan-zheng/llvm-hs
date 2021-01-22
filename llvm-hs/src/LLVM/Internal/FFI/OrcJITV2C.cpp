#include <iostream>

#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/Mangling.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/Module.h>
#include <llvm-c/Core.h>
#include <llvm-c/Orc.h>

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
void LLVM_Hs_IRLayer_addModule(ThreadSafeModule* tsm, JITDylib* dylib, LLVMTargetDataRef dataLayout, IRLayer* il) {
    auto dataLayoutCpp = *unwrap(dataLayout);
    tsm->withModuleDo([&](auto& module) {
        if (module.getDataLayout().isDefault()) {
            module.setDataLayout(dataLayoutCpp);
        }
    });
    // NOTE: Maybe try module cloning?
    llvm::errs() << "LLVM_Hs_IRLayer_add: " << tsm->getModuleUnlocked() << "\n";
    if (Error err = il->add(*dylib, std::move(*tsm))) {
        llvm::errs() << err << "\n";
        exit(1);
    }
    orc::MangleAndInterner Mangle(il->getExecutionSession(), dataLayoutCpp);
    // dylib.addGenerator(
    //   ExitOnErr(orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
    //       dataLayoutCpp.getGlobalPrefix(),
    //       [MainName = Mangle("main")](const orc::SymbolStringPtr &Name) {
    //         return Name != MainName;
    //       })));
    ExitOnError ExitOnErr;
    dylib->addGenerator(
      ExitOnErr(orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
          dataLayoutCpp.getGlobalPrefix())));
    dylib->addGenerator(
      ExitOnErr(orc::DynamicLibrarySearchGenerator::Load(
          "/usr/lib/libSystem.dylib", dataLayoutCpp.getGlobalPrefix())));
    // LLVMOrcCreateCustomCAPIDefinitionGenerator();
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

uintptr_t LLVM_Hs_ExecutionSession_lookup(ExecutionSession* es, JITDylib *dylib, const char* mangledName) {
    llvm::errs() << "LLVM_Hs_ExecutionSession_lookup start\n";
    es->dump(llvm::errs());
    llvm::errs() << "LLVM_Hs_ExecutionSession_lookup next\n";
    if (auto symbolOrErr = es->lookup({dylib}, mangledName)) {
        auto& symbol = *symbolOrErr;
        llvm::errs() << "LLVM_Hs_ExecutionSession_lookup end\n";
        return (uintptr_t)symbol.getAddress();
    } else {
        llvm::errs() << "LLVM_Hs_ExecutionSession_lookup error\n";
        Error err = symbolOrErr.takeError();
        llvm::errs() << err << "\n";
        exit(1);
    }
}

LLVMJITEvaluatedSymbol LLVM_Hs_ExecutionSession_lookupSymbol(ExecutionSession* es, JITDylib *dylib, const char* mangledName) {
    llvm::errs() << "LLVM_Hs_ExecutionSession_lookup start\n";
    // Printing here will show unresolved symbols.
    // es->dump(llvm::errs());
    llvm::errs() << "LLVM_Hs_ExecutionSession_lookup next\n";
    if (auto symbolOrErr = es->lookup({dylib}, mangledName)) {
        es->dump(llvm::errs());
        auto& symbol = *symbolOrErr;
        llvm::errs() << "LLVM_Hs_ExecutionSession_lookup end\n";
        return LLVMJITEvaluatedSymbol{
            symbol.getFlags().getRawFlagsValue(),
            static_cast<uint8_t>(symbol.getAddress())};
    } else {
        Error err = symbolOrErr.takeError();
        llvm::errs() << err << "\n";
        exit(1);
    }
}

}
