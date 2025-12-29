import numpy as np
import resource

target_mb = 200

limit_bytes = (target_mb - 1) * 1024 * 1024  # 204,472,320 bytes

# Set the soft and hard limits for address space
resource.setrlimit(resource.RLIMIT_AS, (limit_bytes-1, limit_bytes))

# Verify the limit was set
soft, hard = resource.getrlimit(resource.RLIMIT_AS)
print(f"Address space limit: soft={soft / 1024**2:.0f} MB, hard={hard / 1024**2:.0f} MB")


try:
    arr = np.zeros(target_mb * 1024 * 1024, dtype=np.uint8)
    arr += 4

    print(f"Allocated exactly {arr.nbytes / 1024**2:.0f} MB")
except Exception as e:
    print(f"We caught the memory allocation error: {e}")
