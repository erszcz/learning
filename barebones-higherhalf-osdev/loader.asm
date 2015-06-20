.global _loader                          # Make entry point visible to linker.
.extern _main                            # _main is defined elsewhere
 
# setting up the Multiboot header - see GRUB docs for details
.set MODULEALIGN,   1<<0                    # align loaded modules on page boundaries
.set MEMINFO,       1<<1                    # provide memory map
.set FLAGS,         MODULEALIGN | MEMINFO   # this is the Multiboot 'flag' field
.set MAGIC,         0x1BADB002              # 'magic number' lets bootloader find the header
.set CHECKSUM,      -(MAGIC + FLAGS)        # checksum required
 
# This is the virtual base address of kernel space. It must be used to convert virtual
# addresses into physical addresses until paging is enabled. Note that this is not
# the virtual address where the kernel image itself is loaded -- just the amount that must
# be subtracted from a virtual address to get a physical address.
.set KERNEL_VIRTUAL_BASE,   0xC0000000                  # 3GB
.set KERNEL_PAGE_NUMBER,    (KERNEL_VIRTUAL_BASE >> 22) # Page directory index of kernel's 4MB PTE.
 
 
    .data
    .align 0x1000
BootPageDirectory:
    # This page directory entry identity-maps the first 4MB of the 32-bit physical address space.
    # All bits are clear except the following:
    # bit 7: PS The kernel page is 4MB.
    # bit 1: RW The kernel page is read/write.
    # bit 0: P  The kernel page is present.
    # This entry must be here -- otherwise the kernel will crash immediately after paging is
    # enabled because it can't fetch the next instruction! It's ok to unmap this page later.
    .long 0x00000083
    .fill KERNEL_PAGE_NUMBER - 1, 4             # Pages before kernel space.
    # This page directory entry defines a 4MB page containing the kernel.
    .long 0x00000083
    .fill 1024 - KERNEL_PAGE_NUMBER - 1, 4      # Pages after the kernel image.
 
 
    .text
    .align 4
MultiBootHeader:
    .long MAGIC
    .long FLAGS
    .long CHECKSUM
 
# reserve initial kernel stack space -- that's 16k.
.set STACKSIZE, 0x4000
 
# setting up entry point for linker
.set loader, _loader - 0xC0000000
.global loader
 
_loader:
    # NOTE: Until paging is set up, the code must be position-independent and use physical
    # addresses, not virtual ones!
    mov ecx, (BootPageDirectory - KERNEL_VIRTUAL_BASE)
    mov cr3, ecx                                        # Load Page Directory Base Register.
 
    mov ecx, cr4
    or ecx, 0x00000010                          # Set PSE bit in CR4 to enable 4MB pages.
    mov cr4, ecx
 
    mov ecx, cr0
    or ecx, 0x80000000                          # Set PG bit in CR0 to enable paging.
    mov cr0, ecx
 
    # Start fetching instructions in kernel space.
    # Since eip at this point holds the physical address of this command (approximately 0x00100000)
    # we need to do a long jump to the correct virtual address of StartInHigherHalf which is
    # approximately 0xC0100000.
    lea ecx, [StartInHigherHalf]
    jmp ecx                                                     # NOTE: Must be absolute jump!
 
StartInHigherHalf:
    # Unmap the identity-mapped first 4MB of physical address space. It should not be needed
    # anymore.
    mov dword ptr [BootPageDirectory], 0
    invlpg [0]
 
    # NOTE: From now on, paging should be enabled. The first 4MB of physical address space is
    # mapped starting at KERNEL_VIRTUAL_BASE. Everything is linked to this address, so no more
    # position-independent code or funny business with virtual-to-physical address translation
    # should be necessary. We now have a higher-half kernel.
    mov esp, stack+STACKSIZE           # set up the stack
    push eax                           # pass Multiboot magic number
 
    # pass Multiboot info structure -- WARNING: This is a physical address and may not be
    # in the first 4MB!
    push ebx
 
    call  _main                  # call kernel proper
    hlt                          # halt machine should kernel return
 
 
    .bss
    .align 32
stack:
    .space STACKSIZE        # reserve 16k stack on a quadword boundary
