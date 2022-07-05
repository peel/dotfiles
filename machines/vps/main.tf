# Deploy instance first
# Add your ssh key into machine through /etc/nixos/configuration.nix
# Deploy using terraform-nixos next

terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~>4.18"
    }
  }
}

provider "aws" {
  region  = "eu-central-1"
  profile = "peel"
}

resource "tls_private_key" "state_key" {
  algorithm = "RSA"
}

resource "local_sensitive_file" "machine_key" {
  content         = tls_private_key.state_key.private_key_pem
  filename        = "${path.module}/sshkey.pem"
  file_permission = 0600
}

resource "aws_security_group" "ssh_and_egress" {
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "wireguard_sg" {
  ingress {
    from_port   = 5553
    to_port     = 5553
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  ingress {
    from_port   = 53
    to_port     = 53
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "apps_sg" {
  ingress {
    from_port   = 32400
    to_port     = 32400
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  ingress {
    from_port   = 993
    to_port     = 993
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "web_sg" {
  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_key_pair" "aws_key" {
  key_name   = "terraform-generated-${sha256(tls_private_key.state_key.public_key_openssh)}"
  public_key = tls_private_key.state_key.public_key_openssh
}

data "tls_public_key" "public_key" {
  private_key_pem = tls_private_key.state_key.private_key_pem
}
resource "local_file" "state_key" {
  content = data.tls_public_key.public_key.public_key_openssh
  filename = "${path.module}/sshkey.pub"
  file_permission = 0600
}

resource "aws_eip" "eip" {
  instance = aws_instance.bastion.id
  vpc      = true
}

resource "aws_instance" "bastion" {
  ami             = "ami-0702eee2e75d541d1" #module.nixos_image.ami
  instance_type   = "t3a.micro"
  key_name        = aws_key_pair.aws_key.key_name
  security_groups = [aws_security_group.ssh_and_egress.name, aws_security_group.web_sg.name, aws_security_group.wireguard_sg.name, aws_security_group.apps_sg.name ]
  root_block_device {
    volume_size = 50 # GiB
  }
}

# module "bastion_deploy" {
#   source = "github.com/numtide/terraform-deploy-nixos-flakes"

#   build_on_target = true
#   target_host = aws_eip.eip.public_ip
#   target_user = "peel"

#   flake      = path.module
#   flake_host = "bastion"

#   ssh_agent = true

#   triggers = {
#     machine_id = aws_instance.bastion.id
#   }
# }

# resource "aws_route53_zone" "px" {
#   name = "fff666.org"
# }

# resource "aws_route53_record" "px" {
#   zone_id = aws_route53_zone.px.zone_id
#   name    = "px.fff666.org"
#   type    = "A"
#   ttl     = "300"
#   records = [aws_eip.eip.public_ip]
# }

output "eip_ip" {
  value = aws_eip.eip.public_ip
}

output "eip_dns" {
  value = aws_eip.eip.public_dns
}
