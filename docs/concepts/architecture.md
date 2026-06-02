# Architecture Overview

## Layers

```
Application
    └── Board Abstraction (firmware/boards/)
            └── Driver API (firmware/drivers/include/)
                    └── Driver Backends (firmware/drivers/src/<mcu>/)
                            └── vendor/CMSIS / Hardware Registers
```

## The Driver Layer

## The Board Layer

## Relationship Between Layers

## What App Developers Touch (and What They Don't)
