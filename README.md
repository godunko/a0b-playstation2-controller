# PlayStation2 controller driver (neutral layer)

This is neutral layer of the driver of the PlayStation2 controller (DualShock). Together with additional transport layer it allows to configure and poll controller, and might be useful in game/robotics applications.

## Transport layer

Implementation of the transport layer highly depends from particular MCU, and moved into separate crates.

 * [STM32F407](https://github.com/godunko/a0b-playstation2_controller-async-stm32f407)
