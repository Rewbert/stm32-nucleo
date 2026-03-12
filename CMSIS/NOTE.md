This subfolder was created by this process

```
git clone <cmsis-5-repo> CMSIS # there is also CMSIS 6, but I am not sure what the difference is
cd CMSIS/Device
mkdir ST
cd ST
git clone git@github.com:STMicroelectronics/cmsis-device-l5.git STM32L5
git clone git@github.com:STMicroelectronics/cmsis-device-u5.git STM32U5
```

We first fetch CMSIS, and then add on ST's board-specific device files. There are CMSIS v5 (and several versions within v5) and v6, but both of these seem to use v5.
