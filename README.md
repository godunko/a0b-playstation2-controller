# PlayStation2 Controller driver (asynchronous neutral layer)

This is neutral layer of the driver of the PlayStation2 Controller (DualShock). Together with additional transport layer it allows to configure and poll controller, and might be useful in game/robotics applications.

It should be used with `light-tasking` or `embedded` GNAT runtimes.

## Transport layer

Implementation of the transport layer highly depends from particular MCU, and moved into separate crates.

 * [STM32F407](https://github.com/godunko/a0b-playstation2_controller-async-stm32f407)
