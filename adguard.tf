terraform {
  required_version = ">= 1.3.0"

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.56"
    }
  }
}

variable "region" {
  type    = string
  default = "us-east-2"
}

provider "aws" {
  profile = "adguard-home"
  region  = var.region
}

resource "aws_security_group" "adguard" {

  # We will be building our NixOS configuration on the target machine, so we
  # permit all outbound connections so that the build can download any missing
  # dependencies.
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  # The "nixos" Terraform module requires SSH access to the machine to deploy
  # our desired NixOS configuration.
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  # Allow port 80 so that we can view our TODO list web page
  ingress {
    from_port   = 3000
    to_port     = 3000
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  # Allow port 433 for https
  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

 # Allow port 853 for dns over quic
  # ingress {
  #   from_port   = 853
  #   to_port     = 853
  #   protocol    = "tcp"
  #   cidr_blocks = ["0.0.0.0/0"]
  # }

  # Allow port 53 so that we can do dns
  ingress {
    from_port   = 53
    to_port     = 53
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  # Allow port 53 so that we can do dns
  ingress {
    from_port   = 53
    to_port     = 53
    protocol    = "udp"
    cidr_blocks = ["0.0.0.0/0"]
  }

}

# Generate an SSH key pair as strings stored in Terraform state
resource "tls_private_key" "adguard" {
  algorithm = "ED25519"
}

# Synchronize the SSH private key to a local file that the "nixos" module can
# use
resource "local_sensitive_file" "ssh_key_file" {
  filename = "${path.module}/id_ed25519"
  content  = tls_private_key.adguard.private_key_openssh
}

# Mirror the SSH public key to EC2 so that we can later install the public key
# as an authorized key for our server
resource "aws_key_pair" "adguard" {
  public_key = tls_private_key.adguard.public_key_openssh
}

module "ami" {
  source  = "github.com/yuanw/terraform-nixos-ng//ami"
  release = "22.11"
  region  = var.region
  system  = "x86_64-linux"
}

resource "aws_instance" "adguard" {
  # This will be an AMI for a stock NixOS server which we'll get to below.
  ami = module.ami.ami

  # We could use a smaller instance size, but at the time of this writing the
  # t3.micro instance type is available for 750 hours under the AWS free tier.
  instance_type = "t3.micro"

  # Install the security groups we defined earlier
  security_groups = [aws_security_group.adguard.name]

  # Install our SSH public key as an authorized key
  key_name = aws_key_pair.adguard.key_name

  # Request a bit more space because we will be building on the machine
  root_block_device {
    volume_size = 7
  }
}

# This ensures that the instance is reachable via `ssh` before we deploy NixOS
resource "null_resource" "wait" {
  provisioner "remote-exec" {
    connection {
      host        = aws_instance.adguard.public_dns
      private_key = tls_private_key.adguard.private_key_openssh
    }

    inline = [":"] # Do nothing; we're just testing SSH connectivity
  }
}

module "nixos" {
  source      = "github.com/yuanw/terraform-nixos-ng//nixos"
  host        = "root@${aws_instance.adguard.public_ip}"
  flake       = ".#adguard"
  arguments   = ["--build-host", "root@${aws_instance.adguard.public_ip}"]
  ssh_options = "-o StrictHostKeyChecking=accept-new -i ${local_sensitive_file.ssh_key_file.filename}"
  depends_on  = [null_resource.wait]
}

output "public_ip" {
  value = aws_instance.adguard.public_ip
}


output "public_dns" {
  value = aws_instance.adguard.public_dns
}
